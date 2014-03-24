#include <iostream>
#include <fstream>
#include <boost/optional/optional.hpp>
#include <boost/variant.hpp>
#include <type_traits>
#include <yaml-cpp/yaml.h>
// 
#include <detector/type.h>
#include <detector/parse.h>

using namespace std;



void yamlparsetest( char* filename) {
  std::ifstream input( filename ) ; 
  YAML::Node doc = YAML::Load(input);

  boost::optional<detector_description_t> mdd = getDetectorDescription(doc) ; 

  if( !mdd.is_initialized() ) return; 

  detector_description_t pdd = mdd.get(); 
  cout << "Name is " << pdd.name << endl; 
  cout << "Description is "<< pdd.description << endl;
  cout << "Reference is " << pdd.reference << endl;

  show_import_or_do( show_name_and_meta_info_efficiency<name_meta_info_efficiency_wrapped<electron_eff_data_t> > 
                   , pdd.object.electron ); 
  show_import_or_do( show_name_and_meta_info_efficiency<name_meta_info_efficiency_wrapped<photon_eff_data_t> >
                   , pdd.object.photon );
  show_import_or_do( show_name_and_meta_info_efficiency<name_meta_info_efficiency_wrapped<bjet_eff_data_t> >
                   , pdd.object.bjet );  
  show_import_or_do( show_name_and_meta_info_efficiency<name_meta_info_efficiency_wrapped<muon_eff_data_t> >
                   , pdd.object.muon );
  show_import_or_do( show_name_and_meta_info_efficiency<name_meta_info_efficiency_wrapped<jet_eff_data_t> >
                   , pdd.object.jet );
  show_import_or_do( taushow, pdd.object.tau ); 
    
  show_import_or_do( ptshow, pdd.object.ptthresholds );

  if( pdd.object.track ) {
    show_import_or_do( show_name_and_meta_info_efficiency<name_meta_info_efficiency_wrapped<track_eff_data_t> >
		       , pdd.object.track.get() );
  }
 
}

/*****************************************/

#ifdef __cplusplus
extern "C" {
#endif 

void testffi( char* filename )
{
  yamlparsetest( filename );
}

#ifdef __cplusplus
}
#endif
