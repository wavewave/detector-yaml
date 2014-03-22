#ifndef __DETECTOR_TYPE__
#define __DETECTOR_TYPE__ 

#include <boost/optional/optional.hpp>
#include <boost/variant.hpp>

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

struct electron_eff_data_t
{ 
  string name;
  meta_info_t meta_info; 
}; 

struct photon_eff_data_t
{ 
  string name;
  meta_info_t meta_info; 
}; 

struct bjet_eff_data_t
{ 
  string name;
  meta_info_t meta_info; 
}; 

struct muon_eff_data_t
{
  string name;
  meta_info_t meta_info;
};

struct jet_eff_data_t
{
  string name;
  meta_info_t meta_info;
};

struct tau_eff_data_t
{
  string name;
  meta_info_t meta_info;
};

/*
struct pt_threshold_eff_data_t
{
  string name;
  meta_info_t meta_info;
};
*/

struct object_description_t
{ 
  either <import,electron_eff_data_t> electron;
  either <import,photon_eff_data_t> photon;
  either <import,bjet_eff_data_t> bjet;
  either <import,muon_eff_data_t> muon;
  either <import,jet_eff_data_t> jet;
  either <import,tau_eff_data_t> tau;
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

class electron_eff_data_wrapper : public INameable, IMetaInfoable 
{
private:
  electron_eff_data_t dat; 
public:  
  electron_eff_data_wrapper( electron_eff_data_t e ) : dat(e) { } 
  string name( void ) { return dat.name; }
  meta_info_t meta_info( void ) { return dat.meta_info; }
};

class muon_eff_data_wrapper : public INameable, IMetaInfoable 
{
private:
  muon_eff_data_t dat; 
public:  
 muon_eff_data_wrapper( muon_eff_data_t e ) : dat(e) { } 
  string name( void ) { return dat.name; }
  meta_info_t meta_info( void ) { return dat.meta_info; }
};


#endif // __DETECTOR_TYPE__

