# syntax=docker/dockerfile:1

# ---- Builder: glibc Node (the purescript npm package ships a prebuilt glibc
# purs binary, so this stage must NOT be alpine) ----
FROM node:22-bookworm-slim AS builder
WORKDIR /app
# spago shells out to git (and needs TLS roots) to resolve/fetch the PureScript
# registry package set; node:22-bookworm-slim ships without git. Install both in
# one cache-friendly layer (before the dependency/source COPYs) and drop the apt
# lists to keep the layer small.
RUN apt-get update \
  && apt-get install -y --no-install-recommends git ca-certificates \
  && rm -rf /var/lib/apt/lists/*
# Lockfile-first for layer caching: deps only re-install when the lockfile changes.
COPY package.json package-lock.json ./
RUN npm ci
# Build inputs (trimmed by .dockerignore). spago.yaml + spago.lock pin the
# PureScript package set; src/ holds the modules; index.html is the entry doc.
COPY spago.yaml spago.lock ./
COPY src ./src
COPY index.html ./
# spago bundle → dist/index.js (needs network for the PureScript package set;
# esbuild inlines the animejs npm dep installed by npm ci).
RUN npm run build

# ---- Runtime: tiny nginx serving the static output ----
FROM nginx:1.27-alpine AS runtime
# Only the static assets the app actually needs at runtime.
COPY --from=builder /app/index.html /usr/share/nginx/html/index.html
COPY --from=builder /app/dist /usr/share/nginx/html/dist
EXPOSE 80
# nginx:alpine ships busybox wget; healthcheck the served root.
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
  CMD wget -qO- http://localhost/ >/dev/null 2>&1 || exit 1
# nginx:alpine's default CMD already runs nginx in the foreground.
