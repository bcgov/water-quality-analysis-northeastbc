
<!-- README.md is generated from README.Rmd. Please edit that file -->
<a rel="Exploration" href="https://github.com/BCDevExchange/docs/blob/master/discussion/projectstates.md"><img alt="Being designed and built, but in the lab. May change, disappear, or be buggy." style="border-width:0" src="https://assets.bcdevexchange.org/images/badges/exploration.svg" title="Being designed and built, but in the lab. May change, disappear, or be buggy." /></a>

[![Lifecycle:Retired](https://img.shields.io/badge/Lifecycle-Retired-d45500)](<Redirect-URL>)

Water quality analysis for Northeast British Columbia
=====================================================

Usage
-----

The raw data is available from the B.C. Data Catalogue under the Open Government Licence - Brisith Columbia: <https://catalogue.data.gov.bc.ca/dataset/bc-environmental-monitoring-system-results>

The data is sourced using the R package `rems`: <https://github.com/bcgov/rems>

There are five core scripts that are required for this water quality characterization; they need to be run in order:

-   01\_load\_ems\_data.R
-   02\_clean\_calc\_WQG.R
-   03\_initial\_vis.R
-   04\_table\_output.R
-   05\_plot\_output.R

Project Status
--------------

The package is under active development.

Getting Help or Reporting an Issue
----------------------------------

To report bugs/issues/feature requests, please file an [issue](https://github.com/bcgov/water_quality_analysis_NortheastBC/issues/).

How to Contribute
-----------------

If you would like to contribute, please see our [CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

License
-------

    Copyright 2017 Province of British Columbia

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at 

       http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.

This repository is maintained by the [ENVWater Team](https://github.com/orgs/bcgov/teams/envwater/members) in the [GitHub BCGov Organization](https://github.com/bcgov).
