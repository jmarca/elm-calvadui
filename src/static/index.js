// pull in desired CSS/SASS files
require( './styles/main.scss' );

var handleColor = require("./handleColor.js")
var handleTopo = require("./handleTopo.js")


// inject bundled Elm app into div#main
var Elm = require( '../elm/Main' );

var app =
        Elm.Main.embed(document.getElementById("app"),{
            'mapfile':'static/data/CA_grid_topology4326.json'
            ,'dataUrl':'static/data'
            ,'year':2012
            ,'month':1
            ,'day':11
        })

app.ports.getColorJson2.subscribe(function(data){
    // console.log('got data with length:',data.length)
    // console.log('data is ', data)

    handleColor(data,
                function(e,colormap){
                    app.ports.colors.send(colormap)
                    return null
                })
    return null
})

app.ports.getTopoJson.subscribe(function(data){
    handleTopo(data,
               function(e,id_path) {
                   app.ports.features.send(id_path)
                   return null
               })
    return null
})
