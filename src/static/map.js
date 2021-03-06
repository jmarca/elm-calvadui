// pull in desired CSS/SASS files
require( './styles/main.scss' );

var handleColor = require("./handleColor.js")
var handleTopo = require("./handleTopo.js")

var d3 = require('d3')
var formatCount = d3.format(".3s")


// inject bundled Elm app into div#main
var Elm = require( '../elm/Main' );

var app =
        Elm.Main.embed(document.getElementById("app"),{
            'mapfile':'data/CA_grid_topology4326.json'
            ,'countymembershipfile':'grid/counties_grids.json'
            ,'airdistrictmembershipfile':'grid/airdistricts_grids.json'
            ,'airbasinmembershipfile':'grid/airbasins_grids.json'
            ,'dataUrl':'hpms/data_by_hr'
            ,'year':2012
            ,'month':1
            ,'day':1
            ,'hour':1
        })

/**
 * getFormattedVMT handler
 * @param {Number} sumvmt
 * @returns {null}
 */
app.ports.getFormattedVMT.subscribe(function(sumvmt){
    app.ports.vmt.send(formatCount(sumvmt))
    return null
})


/**
 * getColorJson2 handler
 * @param {Object} args
 * @param {Integer} args.maxdomain max domain range
 * @param {Array[String,Number]} args.data the data to colorize
 * @returns {}
 */
app.ports.getColorJson2.subscribe(function(args){
    // console.log('got data with length:',data.length)
    // console.log('data is ', data)

    handleColor(args.data,args.maxdomain, args.exponent,
                function(e,colormap,maxdomain){
                    app.ports.colors.send({"data":colormap,
                                           "max":maxdomain})
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
