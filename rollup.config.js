import resolve from '@rollup/plugin-node-resolve'
import commonjs from '@rollup/plugin-commonjs'
import livereload from 'rollup-plugin-livereload'
import { string } from 'rollup-plugin-string'
import typescript from '@rollup/plugin-typescript';

const production = !process.env.ROLLUP_WATCH;

const PATH = 'docs'

function serve() {
    let server;

    function toExit() {
        if (server) server.kill(0);
    }

    return {
        writeBundle() {
            if (server) return;
            server = require('child_process').spawn('node', ['node_modules/node-static/bin/cli.js', PATH], {
                stdio: ['ignore', 'inherit', 'inherit'],
                shell: true
            });

            process.on('SIGTERM', toExit);
            process.on('exit', toExit);
        }
    };
}

export default {
    input: 'src/main.ts',
    output: {
        sourcemap: true,
        format: 'iife',
        name: 'app',
        file: 'docs/build/bundle.js'
    },
    plugins: [
        resolve({
            browser: true
        }),
        string({include: './src/grammar/*.grammar'}),
        commonjs(),
        !production && serve(),
        !production && livereload(PATH),
        typescript(),
    ],
    watch: {
        clearScreen: false
    }
};
