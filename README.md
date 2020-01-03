This Repository contains two projects related to electricity economics: *Forecasting of Electricity Consumption and Optimization of Acquisitions* (in Finnish) and *Electricity Generation and Transmission Problem*. The first project focuses on time series analysis and forecasting, while the second project applies deterministic and stochastic linear optimization in a practical setting.

# Forecasting of Electricity Consumption and Optimization of Acquisitions (in Finnish)

## Abstract
This project is part of Aalto University course MS-C2132 Systeemianalyysilaboratorio I. In this project, we construct a SARIMAX model for forecasting of the electricity consumption of a company. The model is based on 816 electricity consumption measurementents and 840 measurements of outside temperature from the same period of time 31.10.2014 - 3.12.2014. The 24 additional temperature measurements and Box-Jenkins method are used to construct an hourly electricity consumption forecast for thursday 4.12.2014. These forecasts are further used to construct a hourly electricity acquisition strategy for 4.12.2014 by optimizing electricity sales and purchases from various sources.

## Tiivistelmä
Tämä projekti on osa Aalto yliopiston kurssia MS-C2132 Systeemianalyysilaboratorio I. Tässä työssä rakennetaan SARIMAX-malli yrityksen sähkönkulutuksen ennustamista varten. Datana mallin rakentamiseen on 816 sähkönkulutusmittausta kilowattitunteina ja 840 ulkolämpötilaa samalta ajanjakosolta 31.10.2014 - 3.12.2014. Kahdenkymmenenneljän ylimääräisen lämpötilamittauksen avulla, sekä Box-Jenkins -menetelmää hyödyntäen ennustamme torstaille 4.12.2014 tuntikohtaisen sähkönkulutuksen. Ennusteiden perustella luomme tuntikohtaisen sähkönhankitastrategian vuorokaudelle 4.12.2014 optimoimalla sähkön myyntiä ja ostoa tehtävänannon mukaisten sähkönhankitalähteiden puitteissa.

## Authors

- [Kalle Alaluusua](mailto:kalle.alaluusua@aalto.fi)
- Stella Tuovinen

# Electricity Generation and Transmission Problem

## Abstract
This project is part of Aalto university course MS-E2121 Linear Optimization. In this project, we use Julia language to construct two linear programming models, deterministic and stochastic, which optimize investment in electricity generation technologies at five locations, and investments in the transmission capacities between the locations. This is referred to as Generation and Transmission Expansion Planning (GTEP). In order to solve a GTEP, the decision maker should be aware of the demand to be fulfilled and the possible technologies available in each location. Furthermore, the model optimizes the operation decisions, such as the hourly electricity generation and transmission levels at each location. The model involves both annual and operational costs. Five technologies considered are wind, nuclear, coal, gas (OCGT) and gas (CCGT). We are given three scenarios (pessimistic, optimistic and expected), with corresponding hourly demand and wind availability at each node for each season. These are used to construct a deterministic and two-stage stochastic optimization models.

## Authors

- [Kalle Alaluusua](mailto:kalle.alaluusua@aalto.fi)
- Rustam Latypov
