# fuels-craft
Fuels Craft project

Resources: 
---
[Google Drive](https://drive.google.com/drive/folders/1FvHrjbXd3RESTXQ_5tY_81TIiaiZ6TrH)

[Use Cases PowerPoint](https://github.com/user-attachments/files/16732073/FC2FF_Workflows.pptx)

FastFuels Interactive API: https://api.fastfuels.silvxlabs.com/docs

API Key: 114b3607f17d4cf3b3bdc472694aa3b6


The workflow for getting QUIC-Fire data is:
---

Create a domain with a geojson  
Create a tree inventory.  
3a. Create a surface grid.  
3b. Create a tree grid.  
3c. Create a topography grid  
Create a grid export to QUIC-Fire.  

Example API call:  
curl -X 'GET'  
'https://api.fastfuels.silvxlabs.com/v1/domains/?page=0&size=100'  
-H 'accept: application/json'  
-H 'api-key: 114b3607f17d4cf3b3bdc472694aa3b6'  

API Release Notes from Anthony: 
---

I'm happy to announce that we have an official release candidate of the new (version 1) FastFuels API available for our partners to use. I think that you'll find that this version is significantly different from previous versions of the FastFuels API. Hopefully, these changes reflect a greater expressiveness in terms of modeling capabilities and data independence. Our objective moving forward is to provide users with a comprehensive fuel modeling platform that can rapidly ingest new sources of data, provide a menu of modeling options, offer tools for bringing predicted landscapes into alignment with observed landscapes, and output data to a number of different formats (including fire model inputs).

We've included a number of new and revamped products and services with this API release candidate. The full release includes:

FastFuels API v1rc1 - [https://api.fastfuels.silvxlabs.com/]  
This is a JSON based level 2 REST API ready for integration into various modeling systems. API reference documentation is available in static and interactive formats. The full workflow of generating fuels data and bringing everything together in a set of QUIC-Fire .dat input files is fully supported. There are also additional workflows either currently in progress or already integrated into the new API involving additional data sources, improved data independence, and system support modifications and treatments. I'll include more details in a "coming soon" section below.

fastfuels-core - https://github.com/silvxlabs/fastfuels-core  
fastfuels-core is a Python package which serves as the core repository for the algorithms and processes integrated into the API. You can think of fastfuels-core as the foundation on which the API is built. We want this repository to be open so that everyone can see exactly how we're doing things like tree distribution and voxelization, and to foster additional collaboration on the project.

Web Application - https://app.fastfuels.silvxlabs.com/  
The web application is currently the primary source of authentication to the new API. It allows users to create API keys both for personal access, and also for applications. In the near future, we plan on expanding the functionality of the web application to the full set of features available through the API in a user friendly manner. We expect that users integrating FastFuels into systems, or that need a programmatic version of FastFuels will use the API, whereas many users without the technical background in REST APIs will be able to retrieve, visualize, and modify their fuels data via the web application. We encourage our current partners to sign up for the web application as I anticipate moving newsletters, updates, and release notifications to the email list provided through the web application.

Documentation - https://docs.fastfuels.silvxlabs.com/  
Our documentation page contains tutorials and guides for the entire FastFuels platform, including the web application and API. The API currently has complete reference documentation for each endpoint, but this documentation page provides users with information on how to string endpoints together to a desired state. In addition, there is currently content instructing users on how to create API keys and authenticate with the API.

Landing Page - https://fastfuels.silvxlabs.com/  
The landing page provides links to the aforementioned products. We envision this as a place to host publications, newsletters, updates, writings, and more.

Coming Soon (2 weeks - 3 months time frame):

Full treatments module for simulating silvicultural treatments on forest inventory data.  
Expanded shrub and rangeland surface fuel data availability.  
FDS and QES-Fire output formats.  
Ability to create data on the API through file uploads.  
LIDAR and imagery based on-ramp techniques for guiding tree or shrub placement and attribute distributions.  
Ability to create and manage FastFuels data through the web application.  
Additional documentation pages.  
Updated Python SDK for the new API and a Javascript client library.  
Support DUET surface fuel generation.  
Lots more! Email us if you have questions or requests.  

(Anticipated) Frequently Asked Questions  
  
Why a release candidate and not an official release?  
We still have several features planned for the v1 API. While these features are unlikely to result in breaking changes, we want to make sure that we launch v1 without the possibility of breaking changes. In addition, we want our partners to have the opportunity to provide us with feedback before we release v1 and are locked into a design that may not work for everyone.  

Feature x doesn't work. I ran into a bug doing y. It would be great if I could do z.  
Please send an email to support.fastfuels@silvxlabs.com and we'll be happy to help!
