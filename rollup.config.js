import resolve from '@rollup/plugin-node-resolve'
import commonjs from '@rollup/plugin-commonjs'
import livereload from 'rollup-plugin-livereload'
import { string } from 'rollup-plugin-string'

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
            server = require('child_process').spawn('npx', ['node-static', PATH], {
                stdio: ['ignore', 'inherit', 'inherit'],
                shell: true
            });

            process.on('SIGTERM', toExit);
            process.on('exit', toExit);
        }
    };
}

export default {
    input: 'docs/main.js',
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
        string({include: './src/*.grammar'}),
        commonjs(),
        !production && serve(),
        !production && livereload(PATH),
    ],
    watch: {
        clearScreen: false
    }
};
