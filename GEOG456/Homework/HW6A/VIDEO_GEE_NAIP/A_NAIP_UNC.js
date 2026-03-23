// Code starting from: 
// https://code.earthengine.google.com/?scriptPath=Examples%3ADatasets%2FUSDA%2FUSDA_NAIP_DOQQ

var dataset = ee.ImageCollection('USDA/NAIP/DOQQ')
                  .filter(ee.Filter.date('2017-01-01', '2018-12-31'));
var trueColor = dataset.select(['R', 'G', 'B']).median().clip(geometry);
var trueColorVis = {
  min: 0,
  max: 255,
};
Map.setCenter(-105.92901472945381, 35.67653518090711, 15);
Map.addLayer(trueColor, trueColorVis, 'True Color');
//print(trueColor)