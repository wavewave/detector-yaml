#ifndef __DETECTOR_TYPE__
#define __DETECTOR_TYPE__ 

#include <boost/optional/optional.hpp>
#include <boost/variant.hpp>
#include <vector>

using namespace std; 

template<typename A, typename B>
class either 
{
private: 
  enum which_t { left, right } which;
  boost::variant<A,B> content; 
public: 
  void put(A a) { which = left; content = a; } 
  void put(B b) { which = right; content = b; }

  bool isLeft( void ) { return (which == left); }
  bool isRight( void ) { return (which == right); }

  boost::optional<A> getLeft ( void ) 
  {
    if( which == left ) {
      return boost::optional<A>(boost::get<A>(content)); 
    }
    else return boost::optional<A>(); 
  }
  boost::optional<B> getRight( void )
  {
    if( which == right ) 
    {
      return boost::optional<B>(boost::get<B>(content));
    }
    else return boost::optional<B>();
  }
};

struct import
{
  string name;
};

struct meta_info_t
{
  string tag;
  string description;
  string comment; 
  string reference; 
};

struct pt_eta_bins_t 
{
  vector<double> pt;
  vector<double> eta;
};

struct grid_full_t
{
  pt_eta_bins_t pt_eta_bins;
  vector< vector<double> > grid_data; 
};

struct grid_const_t
{
  pt_eta_bins_t pt_eta_bins;
  double value;
};


class grid_t 
{ 
private:
  enum which_t { full, constant } which;
  boost::variant<grid_full_t, grid_const_t> content;
public:  
  void put( grid_full_t g ) { which = full; content = g; }
  void put( grid_const_t g ) { which = constant; content = g; }
  boost::optional<grid_full_t> getGridFull( void ) {
    if( which == full ) 
      return boost::optional<grid_full_t>(boost::get<grid_full_t>(content)); 
    else
      return boost::optional<grid_full_t>();
  }
  boost::optional<grid_const_t> getGridConst( void ) {
    if( which == constant )
      return boost::optional<grid_const_t>(boost::get<grid_const_t>(content));
    else
      return boost::optional<grid_const_t>();
  }
};


struct interpolation_t
{
  string function = "function is not implemented";
};

class pt_eta_data_t 
{
private:
  enum which_t { grid, interpolation } which;
  boost::variant<grid_t, interpolation_t> content; 
public:
  void put(grid_t g) { which = grid; content = g; }
  void put(interpolation_t g ) {which = interpolation; content = g; }
  
  bool isGrid( void ) { return (which == grid); }
  bool isInterpolation(void ) { return (which == interpolation); }
  boost::optional<grid_t> getGrid( void ) {
    if( which == grid ) 
      return boost::optional<grid_t>(boost::get<grid_t>(content)); 
    else
      return boost::optional<grid_t>();
  }
  boost::optional<interpolation_t> getInterpolation( void ) {
    if( which == interpolation )
      return boost::optional<interpolation_t>(boost::get<interpolation_t>(content));
    else
      return boost::optional<interpolation_t>();
  }
};

struct electron_eff_data_t
{ 
  string name;
  meta_info_t meta_info; 
  pt_eta_data_t efficiency; 
}; 

struct photon_eff_data_t
{ 
  string name;
  meta_info_t meta_info; 
  pt_eta_data_t efficiency;
}; 

struct bjet_eff_data_t
{ 
  string name;
  meta_info_t meta_info; 
  pt_eta_data_t efficiency;
  pt_eta_data_t rejection;
}; 

struct muon_eff_data_t
{
  string name;
  meta_info_t meta_info;
  pt_eta_data_t efficiency;
};

struct jet_eff_data_t
{
  string name;
  meta_info_t meta_info;
  pt_eta_data_t efficiency;
};

struct tau_1or3prong_t
{
  pt_eta_data_t efficiency_1prong;
  pt_eta_data_t rejection_1prong;
  pt_eta_data_t efficiency_3prong;
  pt_eta_data_t rejection_3prong;
};

struct tau_combined_t
{
  pt_eta_data_t efficiency; 
  pt_eta_data_t rejection;
};

class tau_eff_detail_t 
{
private:
  enum which_t { tau1or3prong, combined } which;
  boost::variant<tau_1or3prong_t, tau_combined_t> content; 
 public:
  void put(tau_1or3prong_t g) { which = tau1or3prong; content = g; }
  void put(tau_combined_t g ) {which = combined; content = g; }
  
  bool is1or3Prong( void ) { return (which == tau1or3prong); }
  bool isCombined( void ) { return (which == combined); }
  boost::optional<tau_1or3prong_t> get1or3Prong( void ) {
    if( which == tau1or3prong ) 
      return boost::optional<tau_1or3prong_t>(boost::get<tau_1or3prong_t>(content)); 
    else
      return boost::optional<tau_1or3prong_t>();
  }
  boost::optional<tau_combined_t> getCombined( void ) {
    if( which == combined )
      return boost::optional<tau_combined_t>(boost::get<tau_combined_t>(content));
    else
      return boost::optional<tau_combined_t>();
  }
};

struct tau_eff_data_t
{
  string name;
  meta_info_t meta_info;
  string tag_method; 
  tau_eff_detail_t efficiency; 
};

struct track_eff_data_t
{  
  string name;
  meta_info_t meta_info;
  pt_eta_data_t efficiency;
};

struct pt_threshold_eff_data_t
{
  string name;
  double mu_pt_min;
  double electron_pt_min; 
  double photon_pt_min; 
  double jet_pt_min; 
  double bjet_pt_min;
  double track_pt_min;
  double tau_pt_min;
};


struct object_description_t
{ 
  either <import,electron_eff_data_t> electron;
  either <import,photon_eff_data_t> photon;
  either <import,bjet_eff_data_t> bjet;
  either <import,muon_eff_data_t> muon;
  either <import,jet_eff_data_t> jet;
  either <import,tau_eff_data_t> tau;
  boost::optional< either<import, track_eff_data_t> > track;
  either <import,pt_threshold_eff_data_t> ptthresholds;
};

struct detector_description_t
{ 
  string name; 
  string description; 
  string reference; 
  string comment;
  string validation_info;
  object_description_t object;
}; 

class INameable 
{ 
public:
  virtual string name( void ) = 0;  
};

class IMetaInfoable 
{  
public:
  virtual meta_info_t meta_info( void ) = 0;  
};

class IEfficiency
{
public:
  virtual pt_eta_data_t efficiency( void ) = 0;
};


template <typename T> 
class name_meta_info_wrapped : public INameable, IMetaInfoable 
{
private:
  T dat; 
  name_meta_info_wrapped( T e ) : dat(e) { } 

public:  
  virtual string name( void ) { return dat.name; }
  virtual meta_info_t meta_info( void ) { return dat.meta_info; }
  static name_meta_info_wrapped<T> create_wrapped( T e ) { 
    name_meta_info_wrapped<T> t(e); 
    return t;
  }
};

template <typename T> 
class name_meta_info_efficiency_wrapped : public INameable, IMetaInfoable, IEfficiency
{
private:
  T dat; 
  name_meta_info_efficiency_wrapped( T e ) : dat(e) { } 

public:  
  virtual string name( void ) { return dat.name; }
  virtual meta_info_t meta_info( void ) { return dat.meta_info; }
  virtual pt_eta_data_t efficiency( void ) { return dat.efficiency; } 
  static name_meta_info_efficiency_wrapped<T> create_wrapped( T e ) { 
    name_meta_info_efficiency_wrapped<T> t(e); 
    return t;
  }
};

class PTThresholdWrapper {
private:
public:
  pt_threshold_eff_data_t dat ; 
  PTThresholdWrapper( pt_threshold_eff_data_t p ) : dat(p) { }
  static PTThresholdWrapper create_wrapped( pt_threshold_eff_data_t p ) { 
    return PTThresholdWrapper(p);
  }
};

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

#endif // __DETECTOR_TYPE__

