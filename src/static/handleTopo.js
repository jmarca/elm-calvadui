var d3 = require('d3')
d3.tile = require('d3-tile').tile
var width = 500
var height = 500
var pi = Math.PI,
    tau = 2 * pi;


var topojson = require('topojson')

function stringify(scale, translate) {
  var k = scale / 256, r = scale % 1 ? Number : Math.round;
  return "translate(" + r(translate[0] * scale) + "," + r(translate[1] * scale) + ") scale(" + k + ")";
}

function floor(k) {
  return Math.pow(2, Math.floor(Math.log(k) / Math.LN2));
}

function zoomed(projection,raster_g,vector_g,tile){
    var k = projection.k
    var tx = projection.tx
    var ty = projection.ty

    return function(){

        var transform = d3.event.transform

        // console.log('transform is',transform)
        // console.log('original transform is', [k,tx,ty])

        // order matters.  translate first, then scale
        var adjs = transform.translate(tx,ty).scale(k)
        // transform grids, easy

        vector_g.attr("transform",transform)
                .style("stroke-width", 1 / transform.k);
        // now handle tiles
        var newtiles = tile
                .scale( adjs.k )
                .translate([ adjs.x
                             ,adjs.y ])
        console.log('new scale',newtiles.scale())
        console.log('new translate',newtiles.translate())
        var tiles = newtiles()
        console.log(tiles)
        var image = raster_g
                .attr("transform", stringify(tiles.scale, tiles.translate))
                .selectAll("image")
                .data(tiles, function(d) { return d; });
        image.exit().remove();
        image.enter().append("image")
            .attr("xlink:href", function(d) { return "http://" + "abc"[d[1] % 3] + ".tile.openstreetmap.org/" + d[2] + "/" + d[0] + "/" + d[1] + ".png"; })
            .attr("x", function(d) { return d[0] * 256; })
            .attr("y", function(d) { return d[1] * 256; })
            .attr("width", 256)
            .attr("height", 256);

        return null
    }
}

function handleMapTopojson(json,callback){
    var allgrids = json.objects.grids
    // from http://bl.ocks.org/mbostock/5126418
    var land =
            topojson.feature(json,{"type":"GeometryCollection"
                                   ,"geometries":allgrids.geometries})
    // var projection = d3.geoTransverseMercator()
    //         .rotate([124, -32.5])
    //         .fitExtent([[10,10],[width-10,height-10]],land)

    // var path = d3.geoPath()
    //         .projection(projection)

    // Initialize the projection to fit the world in a 1×1 square
    // centered at the origin.
    var projection = d3.geoMercator()
            .scale( 1 / tau )
            .translate([0, 0]);


    // Compute the projected bounding box given a geographic bounding
    // box (here, California).
    //
    // This assumes parallels are horizontal and meridians are vertical…
    // but you could use path.bounds to handle arbitrary shapes.
    //
    // Note that the y-dimension is flipped relative to latitude!
    var bounds = [[-124.408585, 32.534291], [-114.138271, 42.007768]],
        p0 = projection([bounds[0][0], bounds[1][1]]),
        p1 = projection([bounds[1][0], bounds[0][1]]);

    // Convert this to a scale k and translate tx, ty for the projection.
    // For crisp image tiles, clamp to the nearest power of two.
    var k = floor(0.95 / Math.max( (p1[0] - p0[0]) / width
                                 , (p1[1] - p0[1]) / height)),
        tx = (width - k * (p1[0] + p0[0])) / 2,
        ty = (height - k * (p1[1] + p0[1])) / 2;

    projection
        .scale(k / tau)
        .translate([tx, ty]);

    // Lastly convert this to the corresponding tile.scale and tile.translate;
    // see http://bl.ocks.org/mbostock/4150951 for a related example.
    var tile = d3.tile()
            .size([width, height]) // extend a bit from map viewport
            .scale(k)
            .translate([tx, ty])


    var path = d3.geoPath()
            .projection(projection);

    var id_path = land.features.map(function(f){
        var p = f.properties
        var id = (p.i_cell + "_" + p.j_cell)
                .replace(/\.0*/g,"")

        return {"id":id
                ,"path":path(f)}
    })


    var svg, vector_g, raster_g, zoom

    // render the map outline here, handle the coloring in elm
    svg = d3.select("svg")
    raster_g = svg.select("g.tile")
    vector_g = svg.select("g.grid")
    zoom = d3.zoom()
        .scaleExtent( [1,512] )
        //.scaleExtent( [k,1<<18] )
        .on("zoom", zoomed({'k':k
                            ,'tx':tx
                            ,'ty':ty}
                           ,raster_g,vector_g,tile));


    svg.call(zoom)
        .call(zoom.transform, d3.zoomIdentity)


    return callback(null,id_path)
}
module.exports = handleMapTopojson
