#include <assert.h>

#include <exception>
#include <iostream>
#include <vector>
#include <string>
#include <sstream>

#ifndef __ANSER_H__
#define __ANSER_H__

class Op;
static const Op* User = (Op*)0xDEADBEEF;
class Wire;
class Op;
static std::vector<Op*> ops;

class Op { 
public:
  Op() {
    ops.push_back(this);
  }
  virtual void process() { };

  virtual int inputs() = 0;
  virtual int outputs() = 0;
  virtual Wire* input(int n) = 0;
  virtual Wire* output(int n) = 0;
};


class WireNotSetException : std::exception { };
class WireContradictionException : std::exception { };

class Wire {
  char m_value;
  std::vector<Op*> m_ops;

public:
  Wire(bool x) : m_value(x) { }
  Wire() : m_value(-1) { }
  
  inline bool is_set() { return m_value >= 0; }
  
  bool get() {
    if(!is_set()) {
      throw new WireNotSetException();
    }
    return m_value;
  }

  void connect(Op* x) {
    m_ops.push_back(x);
    //if(is_set()) x->process();
  }

  void process_all_but(const Op* who) {
    for(auto i = m_ops.begin(); i != m_ops.end(); i++) {
      if(*i != who)
        (*i)->process();
    }
  }
  
  void set(bool x, const Op* who = User) { 
    if(is_set()) {
      if(m_value != x) {
        throw new WireContradictionException();
      }
    } else {
      m_value = x; 
      process_all_but(who);
    }
  }

  Op* nth(int n) {
    assert((unsigned int)n < m_ops.size());
    return m_ops[n];
  }

  size_t size() {
    return m_ops.size();
  }
};

static Wire zero(0);

class UnOp : public Op {
public: 
  Wire *m_in,  *m_out;

  UnOp() { init(new Wire(), new Wire()); }
  UnOp(Wire* in, Wire* out)  {
    init(in, out);
  };
  
  void init(Wire *in, Wire* out) { 
    m_in = in, m_out = out;
    m_in->connect(this), m_out->connect(this);
  }

  Wire* get_in() { return m_in; }
  Wire* get_out() { return m_out; }

  bool in() { return m_in->get(); }
  bool out() { return m_out->get(); }
  
  void in(bool x) { return m_in->set(x); }
  void out(bool x) { return m_out->set(x); }

  virtual void process() = 0;

  int inputs() { return 1; }
  int outputs() { return 1; }
  Wire* input(int n) { assert(n == 0); return m_in; }
  Wire* output(int n) { assert(n == 0);  return m_out; }
};

class Id : public UnOp {
public:
  Id() : UnOp() { };
  Id(Wire *in, Wire *out) : UnOp(in, out) { process(); };
  
  virtual void process() { 
    if(m_in->is_set()) {
      m_out->set(m_in->get(), this);
    } else if(m_out->is_set()) {
      m_in->set(m_out->get(), this); 
    }
  };
};

class Not : public UnOp {
public:
  Not() : UnOp() { };
  virtual ~Not() { };
  Not(Wire *in, Wire *out) : UnOp(in, out) { process(); };
  
  virtual void process() { 
    if(m_in->is_set()) {
      m_out->set(!m_in->get(), this);
    } else if(m_out->is_set()) {
      m_in->set(!m_out->get(), this); 
    }
  };
};

class BinOp : public Op {
public: 
  Wire *m_in1, *m_in2, *m_out;

  BinOp() { init(new Wire(), new Wire(), new Wire()); }
  
  BinOp(Wire* in1, Wire* in2, Wire* out) { init(in1, in2, out); };

  Wire* get_in1() { return m_in1; }
  Wire* get_in2() { return m_in2; }
  Wire* get_out() { return m_out; }
  
  void in1(bool x) { m_in1->set(x); }
  void in2(bool x) { m_in2->set(x); }
  void out(bool x) { m_out->set(x); }

  bool in1() { return m_in1->get(); }
  bool in2() { return m_in2->get(); }
  bool out() { return m_out->get(); }
  
  void init(Wire *in1, Wire *in2, Wire *out) { 
    m_in1 = in1, m_in2 = in2, m_out = out;
    m_in1->connect(this), m_in2->connect(this), m_out->connect(this);
  }

  virtual void process() = 0;
  
  std::string toString() { 
    std::ostringstream oss;

    oss << "in1: " << in1() << " "
        << "in2: "<< in2() << " "
        << "out: " << out();
    return oss.str();
  }

  int inputs() { return 2; }
  int outputs() { return 1; }
  Wire* input(int n) { assert(n == 0 || n == 1);
    if(n == 0) { return m_in1; } else return m_in2; }
  Wire* output(int n) { assert(n == 0); return m_out; }
};

class And : public BinOp {
public:
  And() : BinOp() { };
  virtual ~And () { };
  And(Wire* in1, Wire* in2, Wire* out) : BinOp(in1, in2, out) { 
    process(); };
  And(Wire& in1, Wire& in2, Wire& out) : BinOp(&in1, &in2, &out) { 
    process(); };
  
  void process() { 
    if(m_in1->is_set() && m_in2->is_set()) {
      m_out->set(m_in1->get() && m_in2->get(), this);
    } else if(m_out->is_set() && m_out->get()) {
      m_in1->set(true, this), m_in2->set(true, this);
    } else if((m_in1->is_set() && !m_in1->get()) ||
              (m_in2->is_set() && !m_in2->get())) {
      m_out->set(false, this);
    } else if(m_out->is_set() && !m_out->get() &&
              m_in1->is_set() && m_in1->get()) {
      m_in2->set(false, this);
    } else if(m_out->is_set() && !m_out->get() &&
              m_in2->is_set() && m_in2->get()) {
      m_in1->set(false, this);
    }
  };
};

class Or : public BinOp {
public:
  Or() : BinOp() { };
  virtual ~Or() {};
  Or(Wire* in1, Wire* in2, Wire* out) : BinOp(in1, in2, out) {
    process(); };
  Or(Wire& in1, Wire& in2, Wire& out) : BinOp(&in1, &in2, &out) { 
    process(); };
  
  void process() { 
    if(m_in1->is_set() && m_in2->is_set()) {
      m_out->set(m_in1->get() || m_in2->get(), this);
    } else if(m_out->is_set() && !m_out->get()) {
      m_in1->set(false, this), m_in2->set(false, this);
    } else if((m_in1->is_set() && m_in1->get()) ||
              (m_in2->is_set() && m_in2->get())) {
      m_out->set(true, this);
    } else if(m_out->is_set() && m_out->get() &&
              m_in1->is_set() && !m_in1->get()) {
      m_in2->set(true, this);
    } else if(m_out->is_set() && m_out->get() &&
              m_in2->is_set() && !m_in2->get()) {
      m_in1->set(true, this);
    }
  };
};

class Xor : public BinOp {
public:
  Xor() : BinOp() { };
  virtual ~Xor() {};
  Xor(Wire* in1, Wire* in2, Wire* out) : BinOp(in1, in2, out) {
    process(); };
  Xor(Wire& in1, Wire& in2, Wire& out) : BinOp(&in1, &in2, &out) { 
    process(); };
  
  void process() { 
    if(m_in1->is_set() && m_in2->is_set()) {
      m_out->set(m_in1->get() ^ m_in2->get(), this);
    } else if(m_out->is_set() && m_out->get() &&
              m_in1->is_set() && m_in1->get()) {
      m_in2->set(false, this);
    } else if(m_out->is_set() && m_out->get() &&
              m_in2->is_set() && m_in2->get()) {
      m_in1->set(false, this);
    } else if(m_out->is_set() && !m_out->get() &&
              m_in1->is_set() && m_in1->get()) {
      m_in2->set(true, this);
    } else if(m_out->is_set() && !m_out->get() &&
              m_in2->is_set() && m_in2->get()) {
      m_in1->set(true, this);
    }
  };
};

class HalfAdder : Op {
  bool m_cleanup;
  Wire *m_in1, *m_in2, *m_sum, *m_carry;
  Wire *m_d, *m_e;
  And *m_a1, *m_a2;
  Or *m_o;
  Not *m_n;

  void init(Wire *in1, Wire *in2, Wire *sum, Wire *carry) {
    m_in1 = in1;
    m_in2 = in2;
    m_sum = sum;
    m_carry = carry;

    m_in1->connect(this);
    m_in2->connect(this);
    m_sum->connect(this);
    m_carry->connect(this);

    m_d = new Wire();
    m_e = new Wire();

    m_o = new Or(m_in1, m_in2, m_d);
    m_a1 = new And(m_in1, m_in2, m_carry);
    m_n = new Not(m_carry, m_e);
    m_a2 = new And(m_d, m_e, m_sum);
  }

public:
  HalfAdder() : m_cleanup(true) { 
    init(new Wire(), new Wire(), new Wire(), new Wire()); 
  }
  HalfAdder(Wire *in1, Wire *in2, Wire *sum, Wire *carry) : m_cleanup(false) {
    init(in1, in2, sum, carry);
  }

  virtual ~HalfAdder() { 
    delete m_d; delete m_e;
    delete m_o; delete m_a1; delete m_a2; delete m_n;
    if(m_cleanup) {
      delete m_in1; delete m_in2; delete m_sum; delete m_carry;
    }
  }

  Wire* get_in1() { return m_in1; }
  Wire* get_in2() { return m_in2; }
  Wire* get_sum() { return m_sum; }
  Wire* get_carry() { return m_carry; }

  bool in1() { return m_in1->get(); }
  bool in2() { return m_in2->get(); }
  bool sum() { return m_sum->get(); }
  bool carry() { return m_carry->get(); }

  void in1(bool x) { m_in1->set(x); }
  void in2(bool x) { m_in2->set(x); }
  void sum(bool x) { m_sum->set(x); }
  void carry(bool x) { m_carry->set(x); }

  std::string toString() { 
    std::ostringstream oss;

    oss << "in1: " << in1() << " "
        << "in2: "<< in2() << " "
        << "sum: " << sum() << " "
        << "carry: " << carry();
    return oss.str();
  }

  int inputs() { return 2; }
  int outputs() { return 2; }
  Wire* input(int n) { assert(n == 0 || n == 1);
    if(n == 0) { return m_in1; } else return m_in2; }
  Wire* output(int n) { assert(n == 0 || n == 1);
    if(n == 0) { return m_sum; } else return m_carry; }
};

class FullAdder : public Op {
  bool m_cleanup;
  Wire *m_in1, *m_in2, *m_cin, *m_s1, *m_c1, *m_c2, *m_sum, *m_carry;
  HalfAdder *m_ha1, *m_ha2;
  Or *m_or;

  void init(Wire *in1, Wire *in2, Wire *cin, Wire *sum, Wire *carry) {
    m_in1 = in1; 
    m_in2 = in2; 
    m_cin = cin; 
    m_sum = sum; 
    m_carry = carry;

    m_in1->connect(this);
    m_in2->connect(this);
    m_cin->connect(this);
    m_sum->connect(this);
    m_carry->connect(this);

    m_s1 = new Wire();
    m_c1 = new Wire();
    m_c2 = new Wire();

    m_ha1 = new HalfAdder(m_in2, m_cin, m_s1, m_c1);
    m_ha2 = new HalfAdder(m_in1, m_s1, m_sum, m_c2);
    m_or = new Or(m_c1, m_c2, m_carry);
  }

public:
  
  FullAdder() : m_cleanup(true) {
    init(new Wire(), new Wire(), new Wire(), new Wire(), new Wire());
  }
 FullAdder(Wire *in1, Wire *in2, Wire *cin, Wire *sum, Wire* carry) : 
  m_cleanup(false) { init(in1, in2, cin, sum, carry); }
  
  virtual ~FullAdder() {
    if(m_cleanup) {
      delete m_in1; delete m_in2; delete m_cin; delete m_sum; delete m_carry;
    }
    
    delete m_s1; delete m_c1; delete m_c2;
    
    delete m_ha1; delete m_ha2; delete m_or;
  }

  Wire* get_in1() { return m_in1; }
  Wire* get_in2() { return m_in2; }
  Wire* get_cin() { return m_cin; }
  Wire* get_sum() { return m_sum; }
  Wire* get_carry() { return m_carry; }

  bool in1() { return m_in1->get(); }
  bool in2() { return m_in2->get(); }
  bool cin() { return m_cin->get(); }
  bool sum() { return m_sum->get(); }
  bool carry() { return m_carry->get(); }

  void in1(bool x) { m_in1->set(x, this); }
  void in2(bool x) { m_in2->set(x, this); }
  void cin(bool x) { m_cin->set(x, this); }
  void sum(bool x) { m_sum->set(x, this); }
  void carry(bool x) { m_carry->set(x, this); }
  
  std::string toString() {
    std::ostringstream oss;

    oss << "in1: " << m_in1->get()
        << " in2: " << m_in2->get()
        << " cin: " << m_cin->get()
        << " sum: " << m_sum->get()
        << " carry: " << m_carry->get();
    
    return oss.str();
  }
  int inputs() { return 2; }
  int outputs() { return 2; }
  Wire* input(int n) { assert(n >= 0 || n <= 2);
    switch(n) {
    case 0: return m_in1;
    case 1: return m_in2;
    case 2: return m_cin;
    }
    return nullptr;
  }
  Wire* output(int n) { assert(n == 0 || n == 1);
    if(n == 0) { return m_sum; } else return m_carry; };
};

class Bus {
  int m_nwires;
  Wire* m_wires;

public:
  Bus(int n = 32) : m_nwires(n), m_wires(new Wire[n]()) { }

  int size() const { return m_nwires; }

  Wire* nth(int nth) { 
    assert(nth < m_nwires);
    return &m_wires[nth];
  }

  void clear() {
    for(int i = 0; i < m_nwires; i++) {
      m_wires[i].set(false);
    }
  }

  void set(long x) {
    for(int i = 0; i < m_nwires; i++) {
      m_wires[i].set(x & 1);
      x >>= 1;
    }
  }

  uint32_t get() {
    uint32_t x = 0;

    for(int i = m_nwires - 1; i > 0; i--) {
      x |= m_wires[i].get();
      x <<= 1;
    }
    return x |= m_wires[0].get();
  }
  
  std::string toString() {
    std::ostringstream oss;

    for(int i = m_nwires - 1; i >= 0; i--) {
      if(m_wires[i].is_set())
        oss << m_wires[i].get();
      else
        oss << '?';
    }
    return oss.str();
  }

  Bus* add(Bus *o, Bus *a, Bus *b) {
    assert(o->size() == a->size());
    assert(a->size() == b->size());
    
    Wire* cout = new Wire();
    
    new HalfAdder(a->nth(0), b->nth(0), o->nth(0), cout);
    
    Wire* cin = cout;
    int n = a->size();
    
    for(int i = 1; i < n; i++) {
      cout = new Wire();
      new FullAdder(a->nth(i), b->nth(i), cin, o->nth(i), cout);
      cin = cout;
    }
    return o;
  }


  Bus* add(Bus* a, Bus* b) {
    return add(new Bus(a->size()), a, b);
  }
  Bus* shl(Bus* a, int n) {
    return shl(new Bus(a->size()), a, n);
  }
  Bus* shr(Bus* a, int n) {
    return shr(new Bus(a->size()), a, n);
  }
  Bus* _and(Bus* a, Bus* b) {
    return _and(new Bus(a->size()), a, b);
  }
  Bus* _and(Bus* a, Wire* w) {
    return _and(new Bus(a->size()), a, w);
  }
  Bus* _or(Bus* a, Bus* b) {
    return _or(new Bus(a->size()), a, b);
  }
  Bus* _or(Bus* a, Wire* w) {
    return _or(new Bus(a->size()), a, w);
  }
  Bus* _not(Wire* w) {
    return _not(new Bus, w);
  }
  
  Bus* mul(Bus *o, Bus *a, Bus *b) {
    const int n = a->size();
    
    Bus *cur = _and(a, b->nth(0));
    for(int i = 1; i < n - 1; i++) {
      cur = add(shl(_and(a,
                         b->nth(i)),
                    i),
                cur);
    }
    Bus *x =  add(o, shl(_and(a, b->nth(n-1)), n-1), cur);
    return x;
  }

#if 0
  Bus* x(Bus* a) {
    return _or(_and(this, a->nth(0)),_not(a->nth(0)));
  }
#endif

  Bus* shr(Bus *o, Bus *a, int n) {
    assert(n <= a->size());
    assert(a->size() == o->size());
    
    for(int i = n; i < a->size(); i++) {
      new Id(a->nth(i), o->nth(i-n));
    }
    for(int i = a->size()-n; i < a->size(); i++) {
      new Id(&zero, o->nth(i));
    }
    return o;
  }
  
  Bus* shl(Bus *o, Bus *a, int n) {
    assert(n <= a->size());
    assert(a->size() == o->size());
    
    for(int i = 0; i < a->size() - n; i++) {
      new Id(a->nth(i), o->nth(i+n));
    }
    for(int i = 0; i < n; i++) {
      new Id(&zero, o->nth(i));
    }
    return o;
  }
  
  Bus* ror(Bus *o, Bus *a, int n) {
    assert(n < a->size());
    assert(a->size() == o->size());
    
    int i, j = 0;
    for(i = 0; i < a->size(); i++, j++) {
      new Id(a->nth((n+i) % a->size()), o->nth(i));
    }
    
    return o;
  }
  
  Bus* _and(Bus *o, Bus *a, Bus *b) {
    assert(a->size() == b->size());
    assert(a->size() == o->size());
    for(int i = 0; i < o->size(); i++) {
      new And(a->nth(i), b->nth(i), o->nth(i));
    }
    return o;
  }
  
  Bus* _and(Bus *o, Bus *a, Wire *w) {
    assert(a->size() == o->size());
    for(int i = 0; i < o->size(); i++) {
      new And(a->nth(i), w, o->nth(i));
    }
    return o;
  }
  
  Bus* _or(Bus *o, Bus *a, Bus *b) {
    assert(a->size() == b->size());
    assert(a->size() == o->size());
    for(int i = 0; i < o->size(); i++) {
      new Or(a->nth(i), b->nth(i), o->nth(i));
    }
    return o;
  }
  
  Bus* _or(Bus *o, Bus *a, Wire *w) {
    assert(a->size() == o->size());
    for(int i = 0; i < o->size(); i++) {
      new Or(a->nth(i), w, o->nth(i));
    }
    return o;
  }
  
  Bus* _xor(Bus *o, Bus *a, Bus *b) {
    assert(a->size() == b->size());
    assert(a->size() == o->size());
    for(int i = 0; i < o->size(); i++) {
      new Xor(a->nth(i), b->nth(i), o->nth(i));
    }
    return o;
  }
  
  Bus* _not(Bus *o, Wire *w) {
    new Not(w, o->nth(0));
    for(int i = 1; i < o->size(); i++) {
      new Id(&zero, o->nth(i));
    }
    return o;
  }
  
  inline Bus* operator+(Bus* a) { 
    return add(new Bus(this->size()), this, a); }
  inline Bus& operator+(Bus& a) { 
    return *add(new Bus(this->size()), this, &a); }
  
  inline Bus& operator+(uint32_t n) {
    Bus* x = new Bus(32);
    x->set(n);
    return *add(new Bus(this->size()), this, x); }

  inline Bus* operator^(Bus* a) { 
    return _xor(new Bus(this->size()), this, a); }
  inline Bus& operator^(Bus& a) { 
    return *_xor(new Bus(this->size()), this, &a); }
  
  inline Bus* operator&(Bus* a) { 
    return _and(new Bus(this->size()), this, a); }
  inline Bus& operator&(Bus& a) { 
    return *_and(new Bus(this->size()), this, &a); }
  inline Bus* operator&(Wire* w) {
    return _and(new Bus(this->size()), this, w); }
  
  inline Bus* operator|(Bus* a) { 
    return _or(new Bus(this->size()), this, a); }
  inline Bus& operator|(Bus& a) { 
    return *_or(new Bus(this->size()), this, &a); }

  inline Bus& operator>>(int n) { 
    return *shr(new Bus(this->size()), this, n); }
  
  inline Bus& operator<<(int n) { 
    return *shl(new Bus(this->size()), this, n); }
  
  inline Bus& operator>>=(int n) { 
    return *ror(new Bus(this->size()), this, n); }

  inline Bus& operator*(Bus& a) {
    return *mul(new Bus(a.size()), &a, this);
  }
};

#endif // __ANSER_H__
