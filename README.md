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
`Electron`, `Photon`, `BJet`, `Muon`, `Jet`, `Tau`, `Track` and `PTThresholds`. Each object can be either imported or 
directly embedded. For ATOM, we generally recommend to import each object in top-level files since it is easier 
to identify each object with their names.

(to be continued)
