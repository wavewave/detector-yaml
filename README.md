detector-yaml
=============

YAML description builder and parser for high energy physics (HEP) colliders 
such as ATLAS and CMS at the Large Hadron Collider (LHC). 

This haskell implementation serves as a prototype definition for the corresponding objects in 
the ATOM project. 

The package depends on `yayaml` which is a haskell YAML builder/parser for general purpose.
It provides a haskel library and two executables: `mkyaml` and `parseyaml`

`mkyaml` generates YAML files defined for the ATLAS and the CMS detector as haskell data structures. 
After execution, top-level detector descriptions will be created in the `top-level` directory and each object 
for identification and smearing will be located in the `object` directory. 

DetectorDescription consists of the following information: `Name`, `Class`, `Description`, `Reference`, 
`Comment`, `ValidationInfo`, `Identification` and `Smearing`. Objects in `Identification` and `Smearing` are
`Electron`, `Photon`, `BJet`, `Muon`, `Jet`, `Tau`, `Track` and `PTThresholds`. 

Each object are a list of items, each of which is either an import or a directly embedded object.  
For ATOM, we generally recommend to import each object in top-level files since it is easier 
to identify each object with their names.

An import object is simply described by `Import: (Name)` where `(Name)` is the name of the object. The object 
should be able to be found in the `object` directory as a YAML file with name `(Name).yaml`. Both of the YAML file 
in the `object` directory and the embedded object have the same format. In the following, we describe the YAML 
format of each object. 

As a common format, the following meta-informations are necessary for every type of objects. 

* *Name* : must be matched with the file name if it is defined in the `object` directory
* *Tag* : 
* *Description* : 
* *Comment* :
* *Reference* :

(to be continued)
