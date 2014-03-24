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

class PTThresholdWrapper {
private:
public:
  pt_threshold_eff_data_t dat ; 
  PTThresholdWrapper( pt_threshold_eff_data_t p ) : dat(p) { }
  static PTThresholdWrapper create_wrapped( pt_threshold_eff_data_t p ) { 
    return PTThresholdWrapper(p);
  }
};

void ptshow( PTThresholdWrapper p ) {
  cout << "name : "            <<  p.dat.name << endl; 
  cout << "mu_pt_min : "       << p.dat.mu_pt_min << endl;
  cout << "electron_pt_min : " << p.dat.electron_pt_min << endl; 
  cout << "photon_pt_min : "   << p.dat.photon_pt_min << endl; 
  cout << "jet_pt_min : "      << p.dat.jet_pt_min << endl;
  cout << "bjet_pt_min : "     << p.dat.bjet_pt_min << endl;
  cout << "track_pt_min : "    << p.dat.track_pt_min << endl; 
  cout << "tau_pt_min : "      << p.dat.tau_pt_min << endl;
}

class TauWrapper : public INameable, public IMetaInfoable {
private:
public:
  tau_eff_data_t dat; 
  TauWrapper( tau_eff_data_t p ) : dat(p) { }
  virtual string name( void ) { return dat.name; }
  virtual meta_info_t meta_info( void ) { return dat.meta_info; } 
  static TauWrapper create_wrapped( tau_eff_data_t p ) {
    return TauWrapper(p);
  }
};

void taushow( TauWrapper p ) {
  show_name_and_meta_info( p );
  cout << "tag_method : " << p.dat.tag_method << endl;
  tau_eff_detail_t eff = p.dat.efficiency; 
  boost::optional<tau_1or3prong_t> mt ;
  boost::optional<tau_combined_t> mtc;
  if( mt = eff.get1or3Prong() ) {
    tau_1or3prong_t t = mt.get();
    show_pt_eta_data( t.efficiency_1prong ); 
    show_pt_eta_data( t.rejection_1prong ); 
    show_pt_eta_data( t.efficiency_3prong ); 
    show_pt_eta_data( t.rejection_3prong ); 
  } else if ( mtc = eff.getCombined() ) {
    tau_combined_t t = mtc.get();
    show_pt_eta_data( t.efficiency );
    show_pt_eta_data( t.rejection );
  }
  cout << "----------------------------------" << endl;
}

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
