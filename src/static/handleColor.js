var d3 = require('d3')

function handleColor(data, callback){
    var c = d3.scalePow().exponent(0.3)
            .domain([0, 300000]) // on one particular
    // hour,156354,
    // so doubling that
            .range([0,1])

    var rainbow = d3.interpolateViridis
    var colormap = {}
    // apply c.interpolateViridis to each incoming thing
    var maxsum = 0
    data.forEach(function(row){
        // records of id:String, value:Numeric
        var cellid = row.id
    cellids.forEach(function(cellid){
        var roaddata = json[cellid]
        var roadtypes = Object.keys(roaddata)

        // sum up sum_vmt and n_mt values
        var sum = roadtypes.reduce(
            function(prev,curr){
                var val = 0
                plotvars.forEach(function (k){
                    if(roaddata[curr][k] !== undefined){
                        val = +roaddata[curr][k]
                    }
                    return null})
                return prev + val
            }, 0)
        maxsum = sum > maxsum ? sum : maxsum
        colormap[cellid] = rainbow(c(sum))
    })
    return callback(null,colormap)
}
module.exports = handleColor
