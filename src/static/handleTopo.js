var d3 = require('d3')
var topojson = require('topojson')

function zoomed(g,mesh){
    return function(){
        var transform = d3.event.transform
        g.attr("transform",transform)
        return null
    }
}

function handleMapTopojson(json,callback){
    var allgrids = json.objects.grids
    // from http://bl.ocks.org/mbostock/5126418
    var land =
            topojson.feature(json,{"type":"GeometryCollection"
                                   ,"geometries":allgrids.geometries})
    var mesh =
            topojson.mesh(json, allgrids, function(a, b) { return a !== b; })

    var path = d3.geoPath()
            .projection(d3.geoTransverseMercator()
                        .rotate([124, -32.5])
                        .fitExtent([[10,10],[480,480]],land))

    var id_path = land.features.map(function(f){
        var p = f.properties
        var id = (p.i_cell + "_" + p.j_cell)
                .replace(/\.0*/g,"")

        return {"id":id
                ,"path":path(f)}
    })
    var svg, g, zoom, mesh

    // render the map outline here, handle the coloring in elm
    svg = d3.select("svg")
    var g = svg.select("g")

    zoom = d3.zoom()
        .scaleExtent( [1, 512])
        .on("zoom", zoomed(g,mesh));

    svg.call(zoom)

    return callback(null,id_path)
}
module.exports = handleMapTopojson
