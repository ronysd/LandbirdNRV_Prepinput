# Landbird_NRV

This repository is an attemp to explore and implement codes for variable extraction, match variables with the required list of variables for individual bird models, and preparing inputs for NRV/FRV calcualtion for the Landbirds

## Purpose

- Extract and process relevant variables from google drive (other sources are work in progress) needed for the model projection.
- Match and format the variables to ensure compatibility with the required model inputs.
- Create interoperable modules for reproducble NRV/FRV calculation

## List of variables needs to be processed:
Total variable 79 (Total source layer 59)
1.	SCANFI: (1km and 5*5): 29
2.	Climate Normal: 24 
3.	Disturbance (Human footprint HF, ALAN): 2+1
4.	Greenup (dormancy, greenup): 2  
5.	Road (1km and 5*5): 2
6.	Topography (TPI, hill, roughness1km): 3
7.	Wetland (peatland, wetlanOccur, Wetseason) (1km and 5*5): 2 +2+2
[Available band names: [occurrence, change_abs, change_norm, seasonality, recurrence, transition, max_extent].]
var dataset = ee.Image('JRC/GSW1_4/GlobalSurfaceWater');
var visualization = {
  bands: ['seasonality'],
  min: 0.0,
  max: 100.0,
  palette: ['ffffff', 'ffbbbb', '0000ff']
};
Map.setCenter(59.414, 45.182, 6);
Map.addLayer(dataset, visualization, 'seasonality');
8.	Annual climate (): 8 NOT AVAILABLE
9.	LCC MODIS: 2


