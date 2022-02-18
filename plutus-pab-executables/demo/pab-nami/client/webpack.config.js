"use strict";

const HtmlWebpackPlugin = require("html-webpack-plugin");
const MiniCssExtractPlugin = require("mini-css-extract-plugin");
const NodePolyfillPlugin = require('node-polyfill-webpack-plugin');

const path = require("path");

const isDevelopment = process.env.NODE_ENV === "development";

const devtool = isDevelopment ? "eval-source-map" : false;

module.exports = {
    experiments: {
      asyncWebAssembly: true,
    },
    devtool,
    devServer: {
        contentBase: path.join(__dirname, "dist"),
        compress: true,
        port: 8009,
        stats: "errors-warnings",
        proxy: {
            "/api": {
                target: "http://localhost:9080",
            },
            // "/ws": {
            //     target: "ws://localhost:9080",
            //     ws: true,
            //     onError (err) {
            //       console.log("Error with the WebSocket:", err);
            //     },
            // },
        },
    },
    entry: "./entry.js",
    output: {
        filename: "[name].[contenthash].js",
        path: path.join(__dirname, "dist"),
        pathinfo: true,
        clean: true,
    },
    optimization: {
        runtimeChunk: 'single',
        splitChunks: {
            cacheGroups: {
                vendor: {
                    test: /[\\/]node_modules[\\/]/,
                    name: 'vendors',
                    chunks: 'all',
                },
            },
        },
    },
    module: {
        rules: [
            {
                test: /\.tsx?$/,
                loader: "ts-loader",
            },
            {
                test: /\.css$/,
                use: [MiniCssExtractPlugin.loader, "css-loader"],
            },
            {
                test: /\.(png|svg|jpg|jpeg|gif)$/i,
                type: "asset/resource",
            },
            {
                test: /\.(woff|woff2|eot|ttf|otf)$/i,
                type: "asset/resource",
            },
        ],
    },
    resolve: {
        modules: [
            "node_modules",
            "lib"
        ],
        extensions: [".js", ".ts", ".tsx"],
        // Used to prevent the error:
        // Module not found: Error: Can't resolve 'fs' in '/home/kolam/git/iog/plutus-starter/demo/pab-nami/client/node_modules/secrets/src'
        fallback: {
            "fs": false
        },
    },
    resolveLoader: {
        modules: [
            "node_modules",
            path.resolve(__dirname, "."),
        ],
    },
    plugins: [
        new HtmlWebpackPlugin({
            template: `${process.env.WEB_COMMON_SRC}/static/index.html`,
            title: "PAB Nami Demo",
            productName: "nami-demo",
            googleAnalyticsId: isDevelopment ? "UA-XXXXXXXXX-X" : "G-9FPZ01J8E4",
            segmentAnalyticsId: isDevelopment ? "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX" : "0CEePM8LJUSpPoo2QGrXHDw4GKg4JFBo",
        }),
        new MiniCssExtractPlugin({
            filename: "[name].[contenthash].css",
        }),
        // allows to use modules from NodeJS like `Buffer` or `Util`.
        new NodePolyfillPlugin(),

    ],
};
