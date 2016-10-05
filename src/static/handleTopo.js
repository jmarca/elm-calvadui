var d3 = require('d3')
d3.tile = require('d3-tile').tile

var topojson = require('topojson')

function stringify(scale, translate) {
  var k = scale / 256, r = scale % 1 ? Number : Math.round;
  return "translate(" + r(translate[0] * scale) + "," + r(translate[1] * scale) + ") scale(" + k + ")";
}

function zoomed(raster_g,vector_g,tile,mesh){
    return function(){
        var transform = d3.event.transform
        vector_g.attr("transform",transform)
        // var tiles = tile
        //         .scale(transform.k)
        //         .translate([transform.x, transform.y])
        // ();

        // var image = raster_g
        //         .attr("transform", stringify(tiles.scale, tiles.translate))
        //         .selectAll("image")
        //         .data(tiles, function(d) { return d; });
        // image.exit().remove();
        // image.enter().append("image")
        //     .attr("xlink:href", function(d) { return "http://" + "abc"[d[1] % 3] + ".tile.openstreetmap.org/" + d[2] + "/" + d[0] + "/" + d[1] + ".png"; })
        //     .attr("x", function(d) { return d[0] * 256; })
        //     .attr("y", function(d) { return d[1] * 256; })
        //     .attr("width", 256)
        //     .attr("height", 256);

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
