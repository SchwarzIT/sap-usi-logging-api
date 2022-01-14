<!-- Links used on this page (Declaration) -->
[WIKI]:           ../../wiki
[CONTRIBUTING]:   ./docs/CONTRIBUTING.md

<!-- Images used on this page (Declaration) -->
[SLG1]: https://github.com/SchwarzIT/sap-usi-logging-api/blob/media/Screenshot_SLG1_Showcase_Data_Containers.png "Showcase Data Containers"




[![SIT](https://img.shields.io/badge/SIT-About%20us-%236e1e6e)](https://it.schwarz)
[![USI](https://img.shields.io/badge/USI-More%20Software-blue)](https://github.com/SchwarzIT/sap-usi)

# USI Logging API
## Purpose
This reusable component allows the creation of complete, comprehensive logs with almost no effort and extends SAP's standard logging API by powerful features.

Logging error situations instead of debugging them can:
* significantly reduce support efforts
* boost developer productivity
* speed up the processing of tickets
* aid in analyzing bugs that are hard to debug

The solution is:
* based on SAPs application log (Transaction SLG1)
* easy to use
* well documentend
* controlled by customizing (Log levels will control the detail level of the logs)
* completely object-oriented
* backwards compatible up to 7.00 and will work on virtually any SAP system

The solution enhances the capabilities of the SAP standard by so-called data containers, that can be used to attach virtually any type of data to log messages making them even more valuable. A variety of data containers for common use cases already exists, but new containers can easily be added whenever needed. The screenshot below shows how seamlessly they are integrated into the SAP standard. Messages with data containers have a detail button that opens a popup and the data can be accessed directly from SLG1.
![alt text][SLG1]

## Further information
[Please refer to the wiki.][WIKI]

## How to contribute
Please check our [contribution guidelines][CONTRIBUTING] to learn more about this topic.