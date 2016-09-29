var d3 = require('d3')

function handleColor(json,callback){
    var c = d3.scalePow().exponent(0.3)
            .domain([0, 300000]) // on one particular
    // hour,156354,
    // so doubling that
            .range([0,1])

    var rainbow = d3.interpolateViridis
    var colormap = {}
    // apply c.interpolateViridis to each incoming thing
    var cellids = Object.keys(json)
    var maxsum = 0
    cellids.forEach(function(cellid){
        var roaddata = json[cellid]
        var roadtypes = Object.keys(roaddata)

        // sum up sum_vmt and n_mt values
        var sum = roadtypes.reduce(
            function(prev,curr){
                var val = 0
                if(roaddata[curr].sum_vmt !== undefined){
                    val = +roaddata[curr].sum_vmt
                }else{
                    if(roaddata[curr].n_mt !== undefined){
                        val = +roaddata[curr].n_mt
                    }}
                return prev + val
            }, 0)
        maxsum = sum > maxsum ? sum : maxsum
        colormap[cellid] = rainbow(c(sum))
    })
    return callback(null,colormap)
}
module.exports = handleColor
