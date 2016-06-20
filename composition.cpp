#include <iostream>
#include <tuple>
#include <type_traits>
#include <utility>
#include <cassert>

using std::cout;
using std::endl;

template<class T>
struct TD;
namespace placeholder {

template <int>
struct arg {};

static constexpr auto _1 = arg<1>{};
static constexpr auto _2 = arg<2>{};
static constexpr auto _3 = arg<3>{};
static constexpr auto _4 = arg<4>{};
static constexpr auto _5 = arg<5>{};
static constexpr auto _6 = arg<6>{};
static constexpr auto _7 = arg<7>{};
static constexpr auto _8 = arg<8>{};
static constexpr auto _9 = arg<9>{};

} // namespace placeholder

namespace xxx {

using std::forward;
using std::is_same;
using std::move;
using std::enable_if_t;
using std::decay_t;
using std::is_convertible;
using placeholder::_1;
using placeholder::_2;
using placeholder::_3;
using placeholder::_4;
using placeholder::_5;
using placeholder::_6;
using placeholder::_7;
using placeholder::_8;
using placeholder::_9;
using placeholder::arg;
using std::tuple;
using std::tuple_element_t;
using std::tuple_cat;
using std::tuple_size;
using std::get;
using std::make_tuple;
using std::true_type;
using std::false_type;
using std::result_of_t;


template <int... Is>
using int_sequence = std::integer_sequence<int, Is...>;

template <int N>
using make_int_sequence = std::make_integer_sequence<int, N>;

template <int N>
using int_constant = std::integral_constant<int, N>;

#define REQUIRES(...) class = enable_if_t<(__VA_ARGS__)>

namespace internal {

  template <int K, class = int_sequence<>>
  struct min_impl
  {
    static constexpr auto value = K;
  };

  template <int K, int I, int... Js>
  struct min_impl<K, int_sequence<I, Js...>>
  {
    static constexpr auto value =
        min_impl<(K < I ? K : I), int_sequence<Js...>>::value;
  };


  template <class>
  struct min;
  template <int I, int... Js>
  struct min<int_sequence<I, Js...>>
      : min_impl<I, int_sequence<Js...>>
  {
  };

  template <int K, class = int_sequence<>>
  struct max_impl
  {
    static constexpr auto value = K;
  };

  template <int K, int I, int... Js>
  struct max_impl<K, int_sequence<I, Js...>>
  {
    static constexpr auto value =
        max_impl<(K > I ? K : I), int_sequence<Js...>>::value;
  };


  template <class>
  struct max;
  template <int I, int... Js>
  struct max<int_sequence<I, Js...>>
      : max_impl<I, int_sequence<Js...>>
  {
  };

  template <class, class>
  struct min_element_impl;

  template <int N, int K, int M>
  struct min_element_impl<int_sequence<N, K>, int_sequence<M>>
  {
    static constexpr auto value = N;
  };

  template <int N, int K, int M, int I, int... Js>
  struct min_element_impl<int_sequence<N, K>,
                          int_sequence<M, I, Js...>>
  {
    static constexpr auto value =
        min_element_impl<int_sequence<(K < I ? N : M),
                                      (K < I ? K : I)>,
                         int_sequence<M + 1, Js...>>::value;
  };


  template <class>
  struct min_element;
  template <int I, int... Js>
  struct min_element<int_sequence<I, Js...>>
      : min_element_impl<int_sequence<0, I>, int_sequence<1, Js...>>
  {
  };

  template <class, class>
  struct remove_element_impl;

  template <int N, int... Is, int J, int... Js>
  struct remove_element_impl<int_sequence<Is...>,
                             int_sequence<N, J, Js...>>
      : remove_element_impl<int_sequence<J, Is...>,
                            int_sequence<N - 1, Js...>>
  {
  };

  template <int... Is, int J, int... Js>
  struct remove_element_impl<int_sequence<Is...>,
                             int_sequence<0, J, Js...>>
  {
    using type = int_sequence<Is..., Js...>;
  };

  template <int, class>
  struct remove_element;
  template <int N, int... Is>
  struct remove_element<N, int_sequence<Is...>>
      : remove_element_impl<int_sequence<>,
                            int_sequence<N, Is...>>
  {
  };

  template <int N, class S>
  using remove_element_t = typename remove_element<N, S>::type;

  template <class Seq>
  struct remove_min_element : remove_element<min_element<Seq>::value, Seq>
  {
  };

  template <class Seq>
  using remove_min_element_t = typename remove_min_element<Seq>::type;

  //////////
  template <int, class, class>
  struct remove_value_impl;

  template <int V, int... Is>
  struct remove_value_impl<V,
                           int_sequence<Is...>,
                           int_sequence<>>
  {
    using type = int_sequence<Is...>;
  };

  template <int V, int... Is, int J, int... Js>
  struct remove_value_impl<V,
                           int_sequence<Is...>,
                           int_sequence<J, Js...>>
      : remove_value_impl<V,
                          int_sequence<Is..., J>,
                          int_sequence<Js...>>
  {
  };
  template <int V, int... Is, int... Js>
  struct remove_value_impl<V,
                           int_sequence<Is...>,
                           int_sequence<V, Js...>>
      : remove_value_impl<V,
                          int_sequence<Is...>,
                          int_sequence<Js...>>
  {
  };
  template <int, class>
  struct remove_value;
  template <int V, int... Is>
  struct remove_value<V, int_sequence<Is...>>
      : remove_value_impl<V, int_sequence<>, int_sequence<Is...>>
  {
  };

  template <int V, class Seq>
  using remove_value_t = typename remove_value<V, Seq>::type;


  template <class, class>
  struct sort_impl;

  template <int... Is>
  struct sort_impl<int_sequence<Is...>, int_sequence<>>
  {
    using type = int_sequence<Is...>;
  };

  template <int... Is, int... Js>
  struct sort_impl<int_sequence<Is...>,
                   int_sequence<Js...>>
      : sort_impl<int_sequence<Is...,
                               min<int_sequence<Js...>>::value>,
                  remove_min_element_t<int_sequence<Js...>>>
  {
  };

  template <class>
  struct sort;
  template <int... Is>
  struct sort<int_sequence<Is...>> : sort_impl<int_sequence<>,
                                               int_sequence<Is...>>
  {
  };

  template <class Seq>
  using sort_t = typename sort<Seq>::type;


  template <int, class, class>
  struct unique_impl;

  template <int I, int... Is>
  struct unique_impl<I, int_sequence<Is...>, int_sequence<>>
  {
    using type = int_sequence<Is...>;
  };

  template <int I, int... Is, int J, int... Js>
  struct unique_impl<I,
                     int_sequence<Is...>,
                     int_sequence<J, Js...>>
      : unique_impl<J,
                    int_sequence<Is..., J>,
                    int_sequence<Js...>>
  {
  };

  template <int... Is, int I, int... Js>
  struct unique_impl<I,
                     int_sequence<Is...>,
                     int_sequence<I, Js...>>
      : unique_impl<I,
                    int_sequence<Is...>,
                    int_sequence<Js...>>
  {
  };

  template <class>
  struct unique;

  template <int I, int... Js>
  struct unique<int_sequence<I, Js...>>
      : unique_impl<I,
                    int_sequence<I>,
                    int_sequence<Js...>>
  {
  };

  template <class Seq>
  using unique_t = typename unique<Seq>::type;


  // meta_sum

  template <int...>
  struct sum_impl;

  template <int I>
  struct sum_impl<I> : int_constant<I>
  {
  };

  template <int I, int... Is>
  struct sum_impl<I, Is...> : int_constant<I + sum_impl<Is...>::value>
  {
  };

  template <int... Is>
  struct sum : sum_impl<Is...>
  {
  };

  template <class T, class U>
  struct decay_placeholder_impl
  {
    using type = U;
  };
  template <int K, class U>
  struct decay_placeholder_impl<arg<K>, U>
  {
    using type = arg<K>;
  };
  template <class T>
  struct decay_placeholder : decay_placeholder_impl<decay_t<T>, T>
  {
  };
  template <class T>
  using decay_placeholder_t = typename decay_placeholder<T>::type;


  template <class>
  struct placeholder_id_impl
  {
    static constexpr int value = 0;
  };

  template <int I>
  struct placeholder_id_impl<arg<I>>
  {
    static constexpr int value = I;
  };

  template <class Arg>
  struct placeholder_id : placeholder_id_impl<decay_t<Arg>>
  {
  };

  template <int... Is>
  constexpr auto int_sequence_size(int_sequence<Is...>)
  {
    return sizeof...(Is);
  }


  template <class F>
  struct is_placeholder_impl : false_type
  {
  };

  template <int I>
  struct is_placeholder_impl<arg<I>> : true_type
  {
  };

  template <class T>
  struct is_placeholder : is_placeholder_impl<decay_t<T>>
  {
  };

  template <class>
  struct has_placeholders : false_type
  {
  };

  template <class F, class... Params>
  struct composable_function;

  template <class>
  struct is_composable_impl : false_type
  {
  };

  template <class F, class... Args>
  struct is_composable_impl<composable_function<F, Args...>> : true_type
  {
  };

  template <class F>
  struct is_composable : is_composable_impl<decay_t<F>>
  {
  };

  template <bool...>
  struct _and : true_type
  {
  };

  template <bool... Vs>
  struct _and<false, Vs...> : false_type
  {
  };

  template <bool... Vs>
  struct _and<true, Vs...> : _and<Vs...>
  {
  };

  template <bool...>
  struct _or : false_type
  {
  };

  template <bool... Vs>
  struct _or<false, Vs...> : _or<Vs...>
  {
  };

  template <bool... Vs>
  struct _or<true, Vs...> : true_type
  {
  };

  template <class... F>
  struct is_evaluated : _and<!is_placeholder<F>::value..., !is_composable<F>::value...>
  {
  };

  template<class... Args>
  struct is_evaluated<tuple<Args...>> : is_evaluated<Args...> {};

  // declaration of meta function
  template <class, class...>
  struct placeholder_seq_impl;

  // recursion termination
  template <int... Is>
  struct placeholder_seq_impl<int_sequence<Is...>>
  {
    using type = int_sequence<Is...>;
  };

  // if it is a generic type, just ignore
  template <int... Is, class T, class... Ts>
  struct placeholder_seq_impl<int_sequence<Is...>,
                              T,
                              Ts...>
      : placeholder_seq_impl<int_sequence<Is...>, Ts...>
  {
  };

  // if it is placeholder, register it
  template <int... Is, int I, class... Ts>
  struct placeholder_seq_impl<int_sequence<Is...>,
                              arg<I>,
                              Ts...>
      : placeholder_seq_impl<int_sequence<Is..., I>, Ts...>
  {
  };

  // if it is invokable, restart recursion and extract placeholders
  template <int... Is, class _F, class... _Args, class... Ts>
  struct placeholder_seq_impl<int_sequence<Is...>,
                              composable_function<_F, _Args...>,
                              Ts...>
      : placeholder_seq_impl<int_sequence<Is...>, decay_t<_Args>..., Ts...>
  {
  };

  template <class... Args>
  struct placeholder_seq : placeholder_seq_impl<int_sequence<>, decay_t<Args>...>
  {
  };

  template <class... Args>
  struct placeholder_seq<tuple<Args...>> : placeholder_seq<Args...>
  {
  };

  template <class... Args>
  using placeholder_seq_t = typename placeholder_seq<Args...>::type;

  template <int, class>
  struct count_if;

  template <int V, int... Is>
  struct count_if<V, int_sequence<Is...>> : sum<(Is == V)...>
  {
  };


  template <int, class>
  struct add_to_seq;
  template <int N, int... Is>
  struct add_to_seq<N, int_sequence<Is...>>
  {
    using type = int_sequence<(Is + N)...>;
  };
  template <int N, class Seq>
  using add_to_seq_t = typename add_to_seq<N, Seq>::type;

  template <class... Args>
  struct placeholders_meta
  {
    using placeholders = placeholder_seq_t<Args...>;
    static constexpr auto count = max<placeholders>::value;
  };


  /////////////////////////////////////////////////
  // implementation for evaluating arguments list

  // forward declare evaluate, needed for one of the specializations below

  template <class...>
  struct construct_args_impl;

  // if we processed all arguments, just return the constructed argument list
  template <>
  struct construct_args_impl<>
  {
    template <class ArgList, class Params>
    auto
    operator()(ArgList&& arg_list, Params&&) const
    {
      return forward<ArgList>(arg_list);
    }
  };

  // generic argument just forwarded
  template <class _Arg, class... _Args>
  struct construct_args_impl<_Arg, _Args...>
  {
    template <class ArgList, class Params, class Arg, class... Args>
    auto
    operator()(ArgList&& arg_list, Params&& params, Arg&& arg, Args&&... args) const
    {
#if 0 // XXX this will copy content from the composable function
      //     thus make it valid in future invokations
      //     this changes semantics of the original function
      auto next = tuple_cat(forward<ArgList>(arg_list),
                            tuple<Arg>{arg}); //forward<Arg>(arg)});
#else // XXX this move content from the composable function
      //     potentially invalidating it
      auto next = tuple_cat(forward<ArgList>(arg_list),
                            tuple<Arg>{forward<Arg>(arg)});
#endif
      return construct_args_impl<_Args...>{}(next,
                                             forward<Params>(params),
                                             forward<Args>(args)...);
    }
  };

  template<class T, class U>
  struct tuple_pushback;

  template<class... Ts, class U>
  struct tuple_pushback<tuple<Ts...>,U> 
  {
    using type = tuple<Ts...,U>;
  };

  template<class T, class U>
  using tuple_pushback_t = typename tuple_pushback<T,U>::type;

  // if the argument is placeholder
  // forward its value from params
  template <int K, class... _Args>
  struct construct_args_impl<arg<K>, _Args...>
  {
    template <class ArgList,
              class Params,
              class Arg,
              class... Args,
              class = result_of_t<
                  construct_args_impl<_Args...>(
                      tuple_pushback_t<decay_t<ArgList>,
                                       tuple_element_t<K - 1,
                                                       decay_t<Params>>>,
                      Params,
                      Args...)>>
    auto
    operator()(ArgList&& arg_list, Params&& params, Arg&&, Args&&... args) const
    {
      using type = tuple_element_t<K-1,decay_t<Params>>;
      auto next = tuple_cat(forward<ArgList>(arg_list),
                            tuple<type>(get<K - 1>(forward<Params>(params))));
      return construct_args_impl<_Args...>{}(next,
                                             forward<Params>(params),
                                             forward<Args>(args)...);
    }
  };
  

  // if the argument is a composable function evaluate its result
  // forward declare evaluation function object
  
  struct evaluate;

  template <class __F, class... __Args, class... _Args>
  struct construct_args_impl<composable_function<__F, __Args...>, _Args...>
  {
    template <class ArgList,
              class Params,
              class Arg,
              class... Args,
              class E = evaluate,
              class = result_of_t<E(__F,Params,tuple<__Args...>)>>
    auto
    operator()(ArgList&& arg_list, Params&& params, Arg&& cf, Args&&... args) const
    {
      decltype(auto) ret = E{}(cf.get_f(),
                               forward<Params>(params),
                               cf.get_args());
      auto next = tuple_cat(forward<ArgList>(arg_list),
                            tuple<decltype(ret)>{ret});
      //                            make_tuple(E{}(cf.get_f(),
      //                                           forward<Params>(params),
      //                                           cf.get_args())));
      return construct_args_impl<_Args...>{}(next,
                                             forward<Params>(params),
                                             forward<Args>(args)...);
    }
  };

  ///////////////////////////////////
  // constructs a new argument list
  //
  struct construct_args
  {
    template <class Params,
              class... Args,
              class = result_of_t<
                  construct_args_impl<decay_t<Args>...>(tuple<>,
                                                        Params,
                                                        Args...)>>
    auto
    operator()(Params&& params, Args&&... args) const
    {
      return construct_args_impl<decay_t<Args>...>{}(tuple<>{},
                                                     forward<Params>(params),
                                                     forward<Args>(args)...);
    }
  };

  ///////////////////////////////////
  // apply arguments to a function

  struct apply
  {
    // use SFINAE to select between the two options:
    //
    // a) returns a composable function if at least one of the arguments
    // requires evaluation

    template <class F,
              class... Args,
              class TupleArgs>
    auto impl(F f, TupleArgs&& args, tuple<Args...>)
    {
      return composable_function<F,Args...>{f,forward<TupleArgs>(args)};
    }
    template <class F,
              class Args,
              int... Is,
              REQUIRES(!is_evaluated<Args>::value)>
    auto operator()(F f, Args&& args, int_sequence<Is...>)
    {
      return impl(f,forward<Args>(args),args);
    }

    template <class F,
              class Args,
              int... Is,
              REQUIRES(is_evaluated<Args>::value),
              class = result_of_t<F(tuple_element_t<Is,decay_t<Args>>...)>>
    auto
    operator()(F f, Args&& args, int_sequence<Is...>)
    {
      using type = decay_t<Args>;
      return f(forward<tuple_element_t<Is,type>>(get<Is>(args))...);
    }
  }; // struct apply

  // b) evaluates function if all of the arguments are evaluated
  
  //////////////////////////////
  // evaluate function helper

  struct evaluate
  {
    struct impl
    {
      template <class F,
                class Params,
                class Args,
                int... Is,
                class = result_of_t<
                    apply(F,
                          result_of_t<
                              construct_args(Params,
                                             tuple_element_t<Is, decay_t<Args>>...)>,
                          make_int_sequence<sizeof...(Is)>)>>
      auto operator()(F f, Params&& params, Args&& args, int_sequence<Is...>)
      {
        return apply{}(f,
                       construct_args{}(forward<Params>(params), get<Is>(args)...),
                       make_int_sequence<sizeof...(Is)>{});
      }
    };

    template <class F,
              class Params,
              class Args,
              class = result_of_t<
                  impl(F,
                       Params,
                       Args,
                       make_int_sequence<tuple_size<decay_t<Args>>::value>)>>
    auto operator()(F f, Params&& params, Args&& args)
    {
      return impl{}(f,
                    forward<Params>(params),
                    args,
                    make_int_sequence<tuple_size<decay_t<Args>>::value>{});
    }
  }; // struct evaluate

  ///////////////////////////////
  // Composable function class

  template <class F, class... Args>
  struct composable_function
  {
    using args_t = tuple<Args...>;
    F const f;
    args_t const args;

    static constexpr auto placeholders_count =
        placeholders_meta<Args...>::count;

    template<class... _Args>
    composable_function(F f, _Args&&... _args)
        : f(f), args{forward<_Args>(_args)...} {}

    F const& get_f() const { return f; }; 
    args_t const& get_args() const { return args; }


    // when calling composable function
    // the # of placeholders must match the # of arguments
    template <class... Params,
              class = enable_if_t<placeholders_count == sizeof...(Params)>,
              class = result_of_t<evaluate(F,tuple<Params...>,args_t)>>
    auto operator()(Params&&... params) const
    {
      return evaluate{}(get_f(),
                        tuple<Params...>(forward<Params>(params)...),
                        get_args());
    }
  };

  template <class F>
  struct composable_function<F> : F
  {
    composable_function(F f) : F(f) {}

    F const& get_f() const { return static_cast<F const&>(*this); }

    template <class... Args,
              class = enable_if_t<!is_evaluated<Args...>::value>>
    auto operator()(Args&&... args) const
    {
      return composable_function<F, Args...>{
          get_f(), forward<Args>(args)...};
    }

    template <class... Args,
              class = enable_if_t<is_evaluated<Args...>::value>,
              class = result_of_t<F(Args...)>>
    auto operator()(Args&&... args) const
    {
      return get_f()(forward<Args>(args)...);
    }
  };

}    // namespace internal

template<class F>
auto make_composable(F f)
{
  return internal::composable_function<F>{f};
}
}    // namespace xxx

int iadd(int a, int b) { return a + b; }
struct add_t
{
  int operator()(int a, int b) const
  {
    return a+b;
  }
};
struct mul_t
{
  int operator()(int a,int b) const
  {
    return a*b;
  }
};

int main(int argc, char*argv[])
{
  using namespace placeholder;
  using xxx::make_composable;

#if 0
    auto add = make_composable([](auto x, auto y) { return x+y; });
    auto mul = make_composable([](auto x, auto y) { return x*y; });
#else
    auto add = make_composable(add_t{});
    auto mul = make_composable(mul_t{});
#endif

#if 0
  {
    struct my3 {};
    auto f = add; //make_composable([=](auto x, auto y) { return argc+x*y; });
    auto ff = my3{};
    auto g = f(_2,_1);
//    auto k = f(_1,g(_1));
//    TD<decltype(g)> t;
//    return k(2);
  }
#endif

#if 0
  {
    auto add = [](int &x, int y) { x++; y++; return x+y;};
    auto f = [=](int& x, int& y) { return add(x,add(y,y)); };
    int i =2 ;
    auto g = f(i,i);
    return i;
  }
#endif
#if 1
  {
    struct my3 {};
//    auto add = make_composable([](auto x, auto y) { return x+y; });
    auto add = make_composable([](int& x, int y) { x++; y++; return x+y; });
//    auto add = make_composable(add_t{});
    auto k1 = add(_1,_2);
    auto f1 = add(_1,add(_3,_3));
    int i = 2;
    auto g1 = f1(i,2,i); 
//    return i;
//    return g1;
//    return g1;
//    TD<decltype(f1)> t1;
 //   auto mul = make_composable([](auto x, auto y) { return x*y; });
//    auto saxpy = add(mul(_1,_2),_3);
//    auto val = saxpy(2,3,_2);

  }
#endif

#if 1
  {
    auto saxpy = add(mul(_1,_2),_3);
//    auto add2x = saxpy(2, 3, _1);
//    cout << "add2x(4)= " << add2x(4) << endl;
//
    auto add2y = saxpy(2, _1, 3);
//    TD<decltype(add2y)> t;
//    TD<decltype(v)> t;
//    auto val = add2y(4);
    cout << "add2y(4)= " << add2y(4) << endl;
#if 1
    auto add2z = saxpy(2, _2, _1);
    auto add2w = saxpy(2, _1, _2);
    auto add3x = saxpy(_3, _2, _1);
    auto add3y = saxpy(_1, _2, _3);
    cout << "add2z(3,4)= " << add2z(3, 4) << endl;
    cout << "add2w(3,4)= " << add2w(3, 4) << endl;
    cout << "add3x(4,3,2)= " << add3x(4,3,2) << endl;
    cout << "add3y(2,3,4)= " << add3y(2,3,4) << endl;
#endif
  }
#endif



#if 1
#if 1
  {
    auto add1 = add(_1, _1);
    //    TD<decltype(add1)> t;
    auto add2 = add1(add1(_1));
    auto val  = add2(3);
//    { TD<decltype(add1)> t1;}
//    { TD<decltype(add2)> t1;}
//    { TD<decltype(val)> t1;}
  //   TD<decltype(add2)> t;
  //   TD<decltype(val)> t1;
      cout << val << endl;
  //    auto add2 = add(_1,add1(_2));
  //    TD<decltype(add2)> t;
  }
#endif

#if 1
  {
    auto sqr = mul(_1,_1);
    auto pow4 = mul(sqr,sqr);
    cout << pow4(2) << endl;
  }
#endif

#if 1
  {
   auto saxpy  /* (a,x,y) = a*x+y */ = add(mul(_1,_2),_3);
   auto saxpy1 /* (a,x) = a*x+a */ = add(mul(_1,_2),_1);
   auto saxpy2  /* (x) = a*x+x */ = add(mul(_1,_2),mul(_1,add(mul(_1,_2),_2)));
   auto val = saxpy(argc*2.0,3.0,4.0);
   auto val1 = saxpy1(argc*2.0,3.0);
   auto val2 = saxpy2(2,3);
   cout << val << endl;
   cout << val1 << endl;
   cout << val2 << endl;
  }
#endif

#if 1
  {
    auto mulx  = mul(_1,2);
    auto saxpy = add(mulx(_1),_2);
//    auto val = mulx(1,2);
    auto val = saxpy(2,3);
//  TD<decltype(saxpy)> t;
//  TD<decltype(val)> t1;
   cout << val << endl;
  }
#endif

#if 1
  {
    auto sum3 = make_composable([](auto a, auto b) { return a+b; });
    auto g = sum3(_1,2);
    auto gval = g(3);
    cout << "g3()= " << gval << endl;

   auto dbl = [=](auto&& x) { return add(x,x); };
   auto cur = dbl(_1);
   auto quad = add(cur(_2),_1);
   auto quad1 = add(cur(_1),_2);
   auto val = quad(2,3);
   auto val1 = quad1(2,3);
//   TD<decltype(val)> t;
   cout << val << endl;
   cout << val1 << endl;
  }
#endif
#endif
  return 0;
}
