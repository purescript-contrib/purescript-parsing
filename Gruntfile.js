module.exports = function(grunt) {

    "use strict";

    grunt.initConfig({ 
    
        clean: ["externs", "js"],
    
        psc: {
            options: {
                main: true
            },
            lib: {
                src:
                    [ "src/**/*.purs.hs"
                    , "examples/**/*.purs.hs"
                    , "bower_components/purescript-*/src/**/*.purs"
                    ],
                dest: "js/Main.js"
            }
        }
        
    });

    grunt.loadNpmTasks("grunt-purescript");
    grunt.loadNpmTasks("grunt-contrib-clean");

    grunt.registerTask("default", ["psc"]);
};
