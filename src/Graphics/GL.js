// WebGL2 renderer with two shader programs:
//   - wireframe (flat color, drawn as GL_LINES)
//   - solid     (Lambert directional + ambient, drawn as GL_TRIANGLES)
//
// PureScript Effect-typed FFI: each export is curried and returns a thunk
// for the final unwrap, matching `Effect a`:
//   foo :: A -> B -> Effect C   <==>   (a) => (b) => () => result

const WIRE_VERT_SRC = `#version 300 es
in vec3 a_position;
uniform mat4 u_model;
uniform mat4 u_projection;
void main() {
  gl_Position = u_projection * u_model * vec4(a_position, 1.0);
}`;

const WIRE_FRAG_SRC = `#version 300 es
precision mediump float;
uniform vec4 u_color;
out vec4 fragColor;
void main() {
  fragColor = u_color;
}`;

const SOLID_VERT_SRC = `#version 300 es
in vec3 a_position;
in vec3 a_normal;
uniform mat4 u_model;
uniform mat4 u_projection;
out vec3 v_normal_world;
void main() {
  v_normal_world = mat3(u_model) * a_normal;
  gl_Position = u_projection * u_model * vec4(a_position, 1.0);
}`;

const SOLID_FRAG_SRC = `#version 300 es
precision mediump float;
in vec3 v_normal_world;
uniform vec4 u_color;
uniform vec3 u_lightDir;
uniform float u_ambient;
out vec4 fragColor;
void main() {
  vec3 n = normalize(v_normal_world);
  float diffuse = max(dot(n, -u_lightDir), 0.0);
  float intensity = u_ambient + (1.0 - u_ambient) * diffuse;
  fragColor = vec4(u_color.rgb * intensity, u_color.a);
}`;

// Hard-coded directional light: travelling down-and-rightward so that
// rotating the cube exposes faces with clearly different brightness on
// every axis (not just the top).
const LIGHT_DIR_RAW = [-0.5, -1.0, -0.3];
const AMBIENT       = 0.2;

function normalize3Static(v) {
  const len = Math.hypot(v[0], v[1], v[2]) || 1.0;
  return [v[0] / len, v[1] / len, v[2] / len];
}

const LIGHT_DIR = normalize3Static(LIGHT_DIR_RAW);

function compileShader(gl, type, src) {
  const sh = gl.createShader(type);
  gl.shaderSource(sh, src);
  gl.compileShader(sh);
  if (!gl.getShaderParameter(sh, gl.COMPILE_STATUS)) {
    const log = gl.getShaderInfoLog(sh);
    gl.deleteShader(sh);
    throw new Error("Graphics.GL: shader compile failed: " + log);
  }
  return sh;
}

function linkProgram(gl, vs, fs) {
  const p = gl.createProgram();
  gl.attachShader(p, vs);
  gl.attachShader(p, fs);
  gl.linkProgram(p);
  if (!gl.getProgramParameter(p, gl.LINK_STATUS)) {
    const log = gl.getProgramInfoLog(p);
    gl.deleteProgram(p);
    throw new Error("Graphics.GL: program link failed: " + log);
  }
  return p;
}

function buildProgram(gl, vertSrc, fragSrc) {
  const vs = compileShader(gl, gl.VERTEX_SHADER,   vertSrc);
  const fs = compileShader(gl, gl.FRAGMENT_SHADER, fragSrc);
  return linkProgram(gl, vs, fs);
}

export const initRenderer = (canvas) => () => {
  // preserveDrawingBuffer lets E2E tests read back rendered pixels deterministically.
  const gl = canvas.getContext("webgl2", { antialias: true, preserveDrawingBuffer: true });
  if (!gl) {
    throw new Error("Graphics.GL: WebGL2 not available on this canvas");
  }

  const wireProgram = buildProgram(gl, WIRE_VERT_SRC,  WIRE_FRAG_SRC);
  const solidProgram = buildProgram(gl, SOLID_VERT_SRC, SOLID_FRAG_SRC);

  const renderer = {
    gl,
    wire: {
      program:       wireProgram,
      locPosition:   gl.getAttribLocation(wireProgram,  "a_position"),
      locModel:      gl.getUniformLocation(wireProgram, "u_model"),
      locProjection: gl.getUniformLocation(wireProgram, "u_projection"),
      locColor:      gl.getUniformLocation(wireProgram, "u_color"),
    },
    solid: {
      program:       solidProgram,
      locPosition:   gl.getAttribLocation(solidProgram,  "a_position"),
      locNormal:     gl.getAttribLocation(solidProgram,  "a_normal"),
      locModel:      gl.getUniformLocation(solidProgram, "u_model"),
      locProjection: gl.getUniformLocation(solidProgram, "u_projection"),
      locColor:      gl.getUniformLocation(solidProgram, "u_color"),
      locLightDir:   gl.getUniformLocation(solidProgram, "u_lightDir"),
      locAmbient:    gl.getUniformLocation(solidProgram, "u_ambient"),
    },
  };

  gl.enable(gl.DEPTH_TEST);
  gl.enable(gl.CULL_FACE);
  gl.cullFace(gl.BACK);
  gl.frontFace(gl.CCW);
  gl.clearColor(1.0, 1.0, 1.0, 1.0);
  gl.viewport(0, 0, canvas.width, canvas.height);

  return renderer;
};

export const createWireframeMesh = (renderer) => (spec) => () => {
  const { gl, wire } = renderer;

  const vao = gl.createVertexArray();
  gl.bindVertexArray(vao);

  const vbo = gl.createBuffer();
  gl.bindBuffer(gl.ARRAY_BUFFER, vbo);
  gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(spec.vertices), gl.STATIC_DRAW);
  gl.enableVertexAttribArray(wire.locPosition);
  gl.vertexAttribPointer(wire.locPosition, 3, gl.FLOAT, false, 0, 0);

  const ibo = gl.createBuffer();
  gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, ibo);
  gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, new Uint16Array(spec.indices), gl.STATIC_DRAW);

  gl.bindVertexArray(null);

  return {
    vao,
    indexCount: spec.indices.length,
    color: [spec.color.r, spec.color.g, spec.color.b, spec.color.a],
  };
};

export const createSolidMesh = (renderer) => (spec) => () => {
  const { gl, solid } = renderer;

  const vao = gl.createVertexArray();
  gl.bindVertexArray(vao);

  const posBuf = gl.createBuffer();
  gl.bindBuffer(gl.ARRAY_BUFFER, posBuf);
  gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(spec.vertices), gl.STATIC_DRAW);
  gl.enableVertexAttribArray(solid.locPosition);
  gl.vertexAttribPointer(solid.locPosition, 3, gl.FLOAT, false, 0, 0);

  const normBuf = gl.createBuffer();
  gl.bindBuffer(gl.ARRAY_BUFFER, normBuf);
  gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(spec.normals), gl.STATIC_DRAW);
  gl.enableVertexAttribArray(solid.locNormal);
  gl.vertexAttribPointer(solid.locNormal, 3, gl.FLOAT, false, 0, 0);

  const ibo = gl.createBuffer();
  gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, ibo);
  gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, new Uint16Array(spec.indices), gl.STATIC_DRAW);

  gl.bindVertexArray(null);

  return {
    vao,
    indexCount: spec.indices.length,
    color: [spec.color.r, spec.color.g, spec.color.b, spec.color.a],
  };
};

export const setClearColor = (renderer) => (color) => () => {
  const { gl } = renderer;
  gl.clearColor(color.r, color.g, color.b, color.a);
};

export const beginFrame = (renderer) => () => {
  const { gl } = renderer;
  gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
};

export const setProjection = (renderer) => (mat) => () => {
  const { gl, wire, solid } = renderer;
  const data = new Float32Array(mat);
  // Upload to both programs so either mesh type renders correctly.
  gl.useProgram(wire.program);
  gl.uniformMatrix4fv(wire.locProjection, true, data);
  gl.useProgram(solid.program);
  gl.uniformMatrix4fv(solid.locProjection, true, data);
};

export const drawMesh = (renderer) => (mesh) => (modelMat) => () => {
  const { gl, wire } = renderer;
  gl.useProgram(wire.program);
  gl.uniformMatrix4fv(wire.locModel, true, new Float32Array(modelMat));
  gl.uniform4fv(wire.locColor, new Float32Array(mesh.color));
  gl.bindVertexArray(mesh.vao);
  gl.drawElements(gl.LINES, mesh.indexCount, gl.UNSIGNED_SHORT, 0);
  gl.bindVertexArray(null);
};

export const drawSolidMesh = (renderer) => (mesh) => (modelMat) => () => {
  const { gl, solid } = renderer;
  gl.useProgram(solid.program);
  // Re-set lighting uniforms every draw call. WebGL stores uniforms per
  // program, so they *should* persist across draws, but setting them
  // unconditionally here is cheap and avoids subtle state-loss bugs.
  gl.uniform3fv(solid.locLightDir, new Float32Array(LIGHT_DIR));
  gl.uniform1f(solid.locAmbient, AMBIENT);
  gl.uniformMatrix4fv(solid.locModel, true, new Float32Array(modelMat));
  gl.uniform4fv(solid.locColor, new Float32Array(mesh.color));
  gl.bindVertexArray(mesh.vao);
  gl.drawElements(gl.TRIANGLES, mesh.indexCount, gl.UNSIGNED_SHORT, 0);
  gl.bindVertexArray(null);
};

export const resizeRenderer = (renderer) => (size) => () => {
  const { gl } = renderer;
  gl.viewport(0, 0, size.width, size.height);
};
