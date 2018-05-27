import fable from 'rollup-plugin-fable';
import fableUtils from 'fable-utils';
import path from 'path';
import prettier from 'rollup-plugin-prettier';

const babelOptions = fableUtils.resolveBabelOptions({
  presets: [],
});

export default [
  /*{
    input: path.resolve(
      __dirname,
      'src/tools/YoloDev.GitVersion.Node/YoloDev.GitVersion.Node.fsproj',
    ),
    output: [
      {
        file: path.resolve(__dirname, 'dist', 'gitversion.js'),
        format: 'cjs',
      },
      {
        file: path.resolve(__dirname, 'dist', 'gitversion.mjs'),
        format: 'es',
      },
    ],
    plugins: [
      fable({ babel: babelOptions, define: ['NODE'] }),
      prettier({ tabWidth: 2, singleQuote: false }),
    ],
    external: ['nodegit', 'fs', 'path', 'os'],
  },*/
  {
    input: path.resolve(
      __dirname,
      'test/node/YoloDev.GitVersion.NodeTest/YoloDev.GitVersion.NodeTest.fsproj',
    ),
    output: [
      {
        file: path.resolve(__dirname, 'dist', 'test.js'),
        format: 'cjs',
      },
      {
        file: path.resolve(__dirname, 'dist', 'test.mjs'),
        format: 'es',
      },
    ],
    plugins: [
      fable({ babel: babelOptions, define: ['NODE'] }),
      prettier({ tabWidth: 2, singleQuote: false }),
    ],
    external: ['nodegit', 'fs', 'path', 'os'],
  },
];
