module.exports = function(grunt) {

  "use strict";

  grunt.initConfig({
    srcFiles: ["purs/**/*.purs"],
    psc: {
      options: {
        modules: ["Chapter2"]
      },
      all: {
        src: ["<%=srcFiles%>"],
        dest: "static/js/phb.js"
      }
    }
  });

  grunt.loadNpmTasks("grunt-purescript");

  grunt.registerTask("default", ["psc:all"]);
};
