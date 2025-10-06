# Bangkok Flood Prediction Project 2011

## Project Overview

This project was developed in October-November 2011 during the **great Thailand floods** to provide real-time flood predictions for residents and authorities in the Bangkok Metropolitan Area (BMA). The predictions helped individuals and communities assess flood risk, determine whether flood protection barriers would be sufficient, and make critical decisions about protecting their homes and evacuating.

### The Problem

During the 2011 floods, the Bangkok area faced a critical information gap:

- **Insufficient monitoring infrastructure**: Lack of monitoring stations and water level measurement points across the metropolitan area
- **No suitable models**: After the flood inundated greater Bangkok, existing hydrological models became ineffective for urban flood prediction
- **Limited situational awareness**: People in flood-affected areas could not determine if water levels were rising or falling
- **No advance forecasting**: The government could not forecast and announce the flood situation in advance due to the complexity of flood movement in urban areas

### The Solution

This project addressed these challenges through a **data-driven forecasting approach**:

- **Self-driving predictions**: Used existing water level data to forecast water levels 5 days ahead
- **Spatial interpolation**: Created flood maps showing water levels across individual neighborhoods
- **Actionable information**: Enabled residents to make informed decisions about barrier reinforcement or relocating valuables
- **Real-time dissemination**: All predictions were released publicly through **WooWooWuu.de** website and **WooWooWuu** Facebook page, created specifically for flood relief

### What People Needed to Know

Residents in flooded areas had simple but critical questions:
- Do I need to prepare more sandbag barriers?
- Should I relocate valuable items to higher floors?
- Will the water level rise or fall in the next few days?
- Will flood walls at 2.5m, 2.8m, or 3.0m MSL be sufficient for my area?

This project empowered individuals to **mitigate their own risk** with hyperlocal flood predictions.

---

## Historical Context

The 2011 Thailand floods were among the worst flooding disasters in the country's history:
- **Affected over 13 million people**
- **Caused estimated damages of $45.7 billion USD**
- **Lasted from July to December 2011**
- **Required large-scale evacuations and emergency responses**

This project represents a real-time scientific response to a major natural disaster, developed under emergency conditions to fill a critical information void.

---

## Project Structure

```
Flood2011/
├── README.md                      # This file
├── docs/
│   ├── CLAUDE.md                  # Technical guide for developers
│   ├── Logo/                      # Project logos and graphics
│   └── reports/                   # Excel reports and official statements
│       └── Statements/            # Official documentation
├── data/
│   ├── raw/                       # Original CSV time series data
│   │   ├── seawl24.csv            # 24-hour sea water level observations
│   │   ├── seaPrd.csv             # Sea level predictions
│   │   └── wloct.csv              # October water level measurements
│   ├── gis/                       # GIS data organized by type
│   │   ├── base_layers/           # Bangkok district boundaries
│   │   ├── bma_output/            # BMA output shapefiles with predictions
│   │   ├── arcgis_projects/       # ArcGIS .mxd map documents
│   │   └── rasters/               # Raster imagery and KML files
│   └── processed/                 # Exported/processed data
│       └── Export/                # Date-specific shapefiles and exports
├── scripts/
│   ├── arima/                     # ARIMA time series prediction models
│   │   ├── arimaBKK.R             # Initial prediction (Oct 21-30, 2011)
│   │   ├── arimaBKK-2.R           # Updated prediction (Nov 8-30, 2011)
│   │   ├── testAR.R               # Model testing and selection
│   │   └── *.csv                  # Historical climate data (GCM, ocean indices)
│   └── spatial/                   # Spatial interpolation scripts
│       ├── interpolateV1.R        # IDW interpolation (initial)
│       ├── interpolateV2.R        # IDW interpolation (improved)
│       └── readSHP.R              # Shapefile utilities
├── output/
│   ├── predictions/               # ARIMA model prediction outputs
│   │   ├── AR_BKK10Nov/           # Nov 10, 2011 predictions
│   │   ├── AR_BKK10Nov2/          # Updated Nov predictions
│   │   └── AR_BKK25oct/           # Oct 25, 2011 predictions
│   ├── maps/                      # Forecast maps and visualizations
│   │   ├── forecasting/           # Daily status maps and animations
│   │   │   ├── BKK*.png           # Daily flood status maps
│   │   │   └── *.gif              # Animated flood progression
│   │   └── IDW*.pdf               # Spatial interpolation outputs
│   └── charts/                    # Water level time series charts
│       ├── WL Charts/             # Water level forecast charts
│       └── WL-ts/                 # Time series visualizations
└── archives/                      # Archived RAR files
```

---

## Methodology

### 1. Time Series Prediction (ARIMA/AR Models)

**Location:** `scripts/arima/`

The project uses **AutoRegressive Integrated Moving Average (ARIMA)** models to forecast water levels:

- **AR (AutoRegressive) Models**: Orders 1-2 tested for simple predictions
- **ARIMA Models**: Various parameter combinations (p,d,q) tested with stepwise and non-stepwise selection
- **ARIMAx (External Regressors)**: Used sea level and ocean climate indices as external predictors for improved accuracy

**Key Scripts:**
- `arimaBKK.R`: Initial prediction model using 25 days of water level data from September 2011
- `arimaBKK-2.R`: Updated model using 18 days of data for November predictions
- `testAR.R`: Comprehensive AR/ARIMA model testing with multiple GCM scenarios and ocean indices

**Data Sources:**
- **Water Level Observations**: Chao Phraya River at Royal Thai Navy Headquarters
- **Sea Level Data**: Tidal predictions and observations (24-hour cycles)
- **Ocean Indices**: ENSO, SST, and other climate indicators (1971-2009)
- **Climate Models**: ECHO-G GCM projections (A1B, A2, B1 scenarios) for robustness testing

**Model Performance:**
- **Nash-Sutcliffe Efficiency (NSE)**: Measures predictive power (-∞ to 1, where 1 = perfect)
- **Root Mean Square Error (RMSE)**: Measures prediction accuracy in meters
- **Forecast Horizon**: 5 days ahead with confidence intervals
- **Validation**: 50% calibration, 50% verification split-sample approach

**Best Performing Model:**
```r
auto.arima(water_level, xreg=sea_level, d=1, stepwise=FALSE)
```
This ARIMAx model with sea level as external regressor provided the most accurate 5-day forecasts.

### 2. Spatial Interpolation

**Location:** `scripts/spatial/`

**Inverse Distance Weighting (IDW)** interpolation creates continuous flood risk maps from point observations:

- **Grid Resolution**: 1000m × 1000m
- **Extension**: 5000m buffer around observation points
- **Input**: Water level measurements from multiple stations across Bangkok
- **Output**: Raster surfaces showing predicted water levels across BMA
- **Method**: `gstat::krige()` function in R

**Key Scripts:**
- `interpolateV1.R`: Initial IDW implementation
- `interpolateV2.R`: Improved version with better grid generation (**recommended**)
- `readSHP.R`: Utilities for reading Bangkok administrative boundaries

**Process:**
1. Collect point measurements from distributed stations
2. Generate uniform grid across BMA
3. Apply IDW algorithm to interpolate between points
4. Overlay Bangkok district boundaries
5. Generate PDF maps showing flood levels by neighborhood

### 3. Flood Risk Assessment

**Critical Thresholds:**
- **2.5m MSL**: General flood protection wall height
- **2.8m MSL**: Top-up wall height
- **3.0m MSL**: Extra top-up wall height

The predictions compared forecasted water levels against these thresholds to assess flood risk for different districts, helping residents determine if their barriers would hold.

### 4. Visualization and Dissemination

**Location:** `output/maps/forecasting/`

Daily flood status maps were generated showing:
- Current water levels
- Predicted water levels (5-day forecast)
- Areas at risk of inundation
- Flow direction and magnitude
- Comparison against flood wall heights

**Map Versions:**
- **V2-V7**: Progressive improvements in accuracy and detail throughout October-November
- **Animations**: GIF files showing flood progression over time
- **Charts**: Time series plots with prediction intervals and confidence bounds

**Public Release:**
All predictions and maps were published on:
- **Website**: WooWooWuu.de
- **Social Media**: WooWooWuu Facebook page

These platforms were created specifically for flood relief and provided free access to predictions for all residents.

---

## Technical Requirements

### R Packages Required

```r
# Time series analysis
library(timeSeries)
library(TSA)
library(forecast)
library(Kendall)
library(Rwave)
library(wmtsa)

# Spatial analysis
library(sp)
library(maptools)
library(gstat)
library(rgdal)

# General utilities
library(plotrix)
library(car)
library(DAAG)
library(lattice)
```

### Software

- **R**: Statistical computing and graphics (version 2.x or higher)
- **ArcGIS**: Spatial mapping and visualization (for .mxd files)
- **Excel**: Data preprocessing and visualization

---

## Running the Models

### ARIMA Predictions

```bash
cd scripts/arima
Rscript arimaBKK.R      # October predictions
Rscript arimaBKK-2.R    # November predictions
```

Output: PDF charts in `output/predictions/AR_BKK*/ARp-charts.pdf`

### Spatial Interpolation

```bash
cd scripts/spatial
Rscript interpolateV2.R
```

Output: Multi-page PDF in `output/maps/` with interpolated flood surfaces

---

## Key Data Files

### Time Series Data

| File | Description | Location |
|------|-------------|----------|
| `seawl24.csv` | 24-hour tidal cycle water level observations (Oct 11-24) | `data/raw/` |
| `seaPrd.csv` | Sea level predictions for future dates | `data/raw/` |
| `wloct.csv` | October water level measurements at Navy HQ | `data/raw/` |
| `wlsep.csv` | September water level data (Days 21-30) | `scripts/arima/` |
| `wlsep2.csv` | September water level data (Days 8-30) | `scripts/arima/` |

### GIS Data

| File | Description | Location |
|------|-------------|----------|
| `bma_output.*` | BMA shapefile with water level attributes | `data/gis/bma_output/` |
| `BKK*.mxd` | ArcGIS map documents for various dates | `data/gis/arcgis_projects/` |

### Reports

| File | Description | Location |
|------|-------------|----------|
| `SeaLevel Navy Headquarter v2.xls` | Comprehensive sea level observations | `docs/reports/` |
| `Status WL*.xls` | Water level status reports (versions 1-10) | `docs/reports/` |

---

## Results and Impact

### Applications

The predictions enabled:
1. **Individual risk mitigation**: Residents could prepare barriers or evacuate based on 5-day forecasts
2. **Community planning**: Neighborhoods coordinated barrier construction and evacuation efforts
3. **Infrastructure protection**: Businesses and institutions protected assets based on predicted water levels
4. **Public awareness**: Daily maps showed which areas were at risk, reducing uncertainty

### Public Dissemination

- **Website**: WooWooWuu.de hosted all daily predictions and maps
- **Facebook**: WooWooWuu page provided updates and answered questions from affected residents
- **Free Access**: All information provided at no cost during the emergency

This grassroots approach filled the information gap when official forecasts were unavailable, empowering individuals to protect themselves and their communities.

---

## Data Sources

1. **Royal Thai Navy Headquarters**: Water level observations from Chao Phraya River
2. **Thai Meteorological Department**: Historical climate data
3. **ECHO-G Global Climate Model**: Future climate projections (1971-2100) - used for model robustness testing
4. **Ocean Climate Indices**: ENSO, SST, and other indicators (1971-2009) - used as external regressors

---

## Important Notes

- All water levels are in **meters above Mean Sea Level (MSL)**
- Predictions were made daily during October-November 2011
- The project combined statistical models (ARIMA) with spatial interpolation (IDW) for comprehensive coverage
- File structure reflects iterative development during the emergency
- Some data files remain in `scripts/arima/` as they are script-specific
- Coordinate system: UTM Zone 47N (projected coordinates)

---

## Author

Project developed in 2011 by Werapol for Bangkok flood prediction and risk assessment.

**Public Platforms:**
- Website: WooWooWuu.de (created for flood relief)
- Facebook: WooWooWuu (flood information and community support)

---

## License

Historical research project. Data sources should be properly attributed if reused.

---

## References

- 2011 Thailand floods: [Wikipedia](https://en.wikipedia.org/wiki/2011_Thailand_floods)
- ARIMA modeling: Box, G.E.P., & Jenkins, G.M. (1976). *Time Series Analysis: Forecasting and Control*
- Spatial interpolation: Li, J., & Heap, A.D. (2008). *A review of spatial interpolation methods for environmental scientists*

---

## Acknowledgments

This project was developed under emergency conditions to serve the people of Bangkok during one of Thailand's worst natural disasters. Special thanks to all who contributed data, provided feedback, and helped disseminate the predictions to those in need.
