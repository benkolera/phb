module.exports = function(grunt) {

    "use strict";

    grunt.initConfig({
        srcFiles: [
            "purs/**/*.purs",
            "static/components/**/*.purs"
        ],
        psc: {
            options: {
                modules: ["Phb","PleaseJs"]
            },
            all: {
                src: ["<%=srcFiles%>"],
                dest: "static/js/phb.js"
            }
        },
        watch: {
            files: ["<%=srcFiles%>"],
            tasks: ["psc:all"]
        }
    });

    grunt.loadNpmTasks("grunt-purescript");
    grunt.loadNpmTasks("grunt-contrib-watch");

    grunt.registerTask("default", ["psc:all"]);
};
