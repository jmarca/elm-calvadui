var d3 = require('d3')

/**
 * handleColor
 *
 * @param {Array[Array]} data the data to color.  An array of arrays.
 *   Each of nested arrays has two values: first value is cellid, second
 *   is the float value to use for assigning a color
 * @param {} callback
 * @returns {}
 */
function handleColor(data, callback){
    //console.log('here in handle color')
    // passed array of arrays.  Array[0] is cellid, array[1] is value
    var c = d3.scalePow().exponent(0.3)
            .domain([0, 190000]) // on one particular
    // hour,156354,
    // so doubling that
            .range([0,1])

    var rainbow = d3.interpolateViridis
    var colormap = {}
    // apply c.interpolateViridis to each incoming thing
    var maxsum = 0
    data.forEach(function(row){
        // records of id:String, value:Numeric
        var cellid = row[0]
        var sum = row[1]
        maxsum = sum > maxsum ? sum : maxsum
        colormap[cellid] = rainbow(c(sum))
    })
    console.log(maxsum)
    return callback(null,colormap)
}
module.exports = handleColor
