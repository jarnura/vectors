// WebGL2 wireframe renderer.
//
// PureScript Effect-typed FFI convention: each export is curried and returns
// a thunk for the final unwrap, matching `Effect a` shape:
//   foo :: A -> B -> Effect C   <==>   (a) => (b) => () => result

const VERT_SRC = `#version 300 es
in vec3 a_position;
uniform mat4 u_model;
uniform mat4 u_projection;
void main() {
  gl_Position = u_projection * u_model * vec4(a_position, 1.0);
}`;

const FRAG_SRC = `#version 300 es
precision mediump float;
uniform vec4 u_color;
out vec4 fragColor;
void main() {
  fragColor = u_color;
}`;

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

export const initRenderer = (canvas) => () => {
  const gl = canvas.getContext("webgl2", { antialias: true });
  if (!gl) {
    throw new Error("Graphics.GL: WebGL2 not available on this canvas");
  }
  const vs = compileShader(gl, gl.VERTEX_SHADER,   VERT_SRC);
  const fs = compileShader(gl, gl.FRAGMENT_SHADER, FRAG_SRC);
  const program = linkProgram(gl, vs, fs);
  gl.useProgram(program);

  const renderer = {
    gl,
    program,
    locPosition:   gl.getAttribLocation(program,  "a_position"),
    locModel:      gl.getUniformLocation(program, "u_model"),
    locProjection: gl.getUniformLocation(program, "u_projection"),
    locColor:      gl.getUniformLocation(program, "u_color"),
  };

  gl.enable(gl.DEPTH_TEST);
  gl.clearColor(1.0, 1.0, 1.0, 1.0);
  gl.viewport(0, 0, canvas.width, canvas.height);

  return renderer;
};

export const createWireframeMesh = (renderer) => (spec) => () => {
  const { gl, locPosition } = renderer;

  const vao = gl.createVertexArray();
  gl.bindVertexArray(vao);

  const vbo = gl.createBuffer();
  gl.bindBuffer(gl.ARRAY_BUFFER, vbo);
  gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(spec.vertices), gl.STATIC_DRAW);
  gl.enableVertexAttribArray(locPosition);
  gl.vertexAttribPointer(locPosition, 3, gl.FLOAT, false, 0, 0);

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

export const beginFrame = (renderer) => () => {
  const { gl } = renderer;
  gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
};

export const setProjection = (renderer) => (mat) => () => {
  const { gl, locProjection } = renderer;
  // transpose=true so PureScript can pass row-major matrices natively.
  gl.uniformMatrix4fv(locProjection, true, new Float32Array(mat));
};

export const drawMesh = (renderer) => (mesh) => (modelMat) => () => {
  const { gl, locModel, locColor } = renderer;
  gl.uniformMatrix4fv(locModel, true, new Float32Array(modelMat));
  gl.uniform4fv(locColor, new Float32Array(mesh.color));
  gl.bindVertexArray(mesh.vao);
  gl.drawElements(gl.LINES, mesh.indexCount, gl.UNSIGNED_SHORT, 0);
  gl.bindVertexArray(null);
};

export const resizeRenderer = (renderer) => (size) => () => {
  const { gl } = renderer;
  gl.viewport(0, 0, size.width, size.height);
};
