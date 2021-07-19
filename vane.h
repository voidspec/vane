/*
VANE    2017 July 19


MIT License


Copyright (c) 2017 voidspec (voidspec@gmail.com)

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/
#ifndef ___VANE_H_20170719
#define ___VANE_H_20170719

#include <type_traits>
#include <array>
#include <algorithm>
#include <utility>
#include <tuple>
#include <iterator>
#include <typeinfo>
#include <exception>
#include <unordered_map>
#include <functional>
#include <vector>
#include <typeinfo>
#include <numeric>
#include <memory>
#include <stdexcept>
#include <assert.h>
#if defined(__CYGWIN__)
#   include <w32api/windows.h>
#elif defined (__MINGW32__)
#   include <windows.h>
#endif

namespace vane {/////////////////////////////////////////////////////////////////////////////////////////////

#ifdef  __GNUC__
#   define __vane_forceinline   inline  __attribute__((__always_inline__))
#   define __vane_noinline              __attribute__ ((__noinline__))
#   ifndef  __forceinline
#       define __forceinline    __vane_forceinline
#   endif
#   define __vane_novtable
#elif   defined(_MSC_VER)
#   define __vane_forceinline   inline  __forceinline
#   define __vane_noinline      __declspec(noinline)
#   define __vane_novtable      __declspec(novtable)
#endif


template<typename...> using __void_t = void;
template<typename T,T...> using void_v = void;

/*--------------------------------------------
    make_static<Ts...>
*/
template <typename...Ts> struct make_static;


/*--------------------------------------------
    is_tuple<T>::value
*/
template<typename T>
struct is_tuple {
    enum { value = false };
};

template<typename...Ts>
struct is_tuple<std::tuple<Ts...>> {
    enum { value = true };
};

template <typename T>
using is_tuple_v = typename is_tuple<T>::value;

/*--------------------------------------------
    multi_array<int, 3,4,5> a;
*/
namespace __helper__ {
    template<typename T, size_t...Ns>
    struct _marray;

    template<typename T, size_t N, size_t...Ns>
    struct _marray<T,N,Ns...> : _marray<T,Ns...> {
        using type = std::array<typename _marray<T,Ns...>::type, N>;
    };

    template<typename T, size_t N>
    struct _marray<T,N> {
        using type = std::array<T,N>;
    };
}

template <typename T, size_t...Ns>
using multi_array = typename __helper__::_marray<T,Ns...>::type;


/*--------------------------------------------
    multi_array_rank<array>
*/
template<typename T>
struct multi_array_rank {
    enum { value = 0 };
};

template<typename T, size_t N>
struct multi_array_rank<std::array<T,N>> {
    enum { value = 1 + multi_array_rank<T>::value };
};

template<typename T, size_t N>
struct multi_array_rank<const std::array<T,N>> {
    enum { value = 1 + multi_array_rank<T>::value };
};

template <typename T>
constexpr int multi_array_rank_v = multi_array_rank<T>::value;

/*--------------------------------------------
    multi_array_getAt
*/
namespace __helper__ {
    template<typename T>
    struct __multi_array_wrap {
        using Data = T;
        static constexpr
        Data &at(T &a) {
            return a;
        }
    };
    template<typename T, size_t N>
    struct __multi_array_wrap<std::array<T,N>> {
        using Data = typename __multi_array_wrap<T>::Data;

        template<typename I,typename...Is>
        static constexpr
        Data &at(std::array<T,N> &a, I i, Is...is) {
            return __multi_array_wrap<T>::at(a[i], is...);
        }
    };
    template<typename T, size_t N>
    struct __multi_array_wrap<const std::array<T,N>> {
        using Data = typename __multi_array_wrap<const T>::Data;

        template<typename I,typename...Is>
        static constexpr
        Data &at(const std::array<T,N> &a, I i, Is...is) {
            assert( 0 <= i );
            assert( i <  (int)N );
            return __multi_array_wrap<const T>::at(a[i], is...);
        }
    };
}//end __helper__

template<typename MA, typename...Is> inline
typename __helper__::__multi_array_wrap<MA>::Data &
multi_array_getAt(MA &a, Is...is) {
    return __helper__::__multi_array_wrap<MA>::at(a, is...);
};


template <typename MA, typename TI, size_t...Is> inline
typename __helper__::__multi_array_wrap<MA>::Data &
__ma_helper_get(MA &a, const std::array<TI,sizeof...(Is)> &ai, std::index_sequence<Is...>) {
    return __helper__::__multi_array_wrap<MA>::at(a, ai[Is]...);
}

template <typename MA, typename AI> inline
const typename __helper__::__multi_array_wrap<MA>::Data &
multi_array_getAt(const MA &a, const AI &ai) {
    return __ma_helper_get(a, ai, std::make_index_sequence<multi_array_rank<MA>::value>());
}

template <typename MA, typename AI> inline
typename __helper__::__multi_array_wrap<MA>::Data &
multi_array_getAt(MA &a, const AI &ai) {
    return __ma_helper_get(a, ai, std::make_index_sequence<multi_array_rank<MA>::value>());
}



/*--------------------------------------------
    __varray2d<bool>
*/
#pragma pack(push,1)
template<typename ET=unsigned char>
struct __varray2d
{
    using element_type = ET;
    int size() const { return _size; }

    int _size;
    ET  _data[0];

    const ET *data() const { return _data; }
    ET *data() { return _data; }

    const ET *operator[](int i) const { 
        assert( 0<=i );
        assert( i < _size );
        return _data + i*_size;
    }
    ET *operator[](int i) { 
        assert( 0<=i );
        assert( i < _size );
        return _data + i*_size;
    }
};
#pragma pack(pop)


/*----------------------------------------------------------------*/
template<typename>
struct resolve_signature;

template<typename R, typename...Ts>
struct resolve_signature<R(Ts...)> {
    using return_type = R;
    using arg_types   = std::tuple<Ts...>;
};

template<typename R, typename Args>
struct make_signature;

template<typename R, typename...Ts>
struct make_signature<R, std::tuple<Ts...>> {
    using type = R(Ts...);
    using return_type = R;
    using arg_types   = std::tuple<Ts...>;
};


/*--------------------------------------------------------------------------
    remove_pointer_or_reference_t<void*>
*/
template<typename T>
struct remove_pointer_or_reference { using type = T; };

template<typename T>
struct remove_pointer_or_reference<T*> { using type = T; };

template<typename T>
struct remove_pointer_or_reference<T&> { using type = T; };

template<typename T>
struct remove_pointer_or_reference<T&&> { using type = T; };

template<typename T>
using remove_pointer_or_reference_t = typename remove_pointer_or_reference<T>::type;

/*--------------------------------------------------------------------------
remove_base_const<T>
*/
template<typename __T>
struct remove_base_const                { using type = __T; };

template<typename __T>
struct remove_base_const<const __T>     { using type = __T; };

template<typename __T>
struct remove_base_const<const __T*>    { using type = __T*; };

template<typename __T>
struct remove_base_const<const __T&>    { using type = __T&; };

template<typename __T>
struct remove_base_const<const __T&&>   { using type = __T&&; };

template<typename __T>
using remove_base_const_t = typename remove_base_const<__T>::type;



/*------------------------------------------------------------------------------
    basetype_xxx's
*/
template<typename B, typename D>
struct basetype_is_base_of
    : std::is_base_of<remove_pointer_or_reference_t<B>,remove_pointer_or_reference_t<D>> { };

template<typename T>
struct basetype_is_polymorphic
    : std::is_polymorphic<remove_pointer_or_reference_t<T>> { };



/*--------------------------------------------------------------------------
    utils
*/

template <typename F, size_t Depth, typename IndexArray, typename TI, size_t R=Depth>
struct __Foreach_cartesian {
    static
    void go(IndexArray &a, F f, const IndexArray &uppers, const IndexArray &lowers) {
        TI end = uppers[Depth-R];
        for(TI i=lowers[Depth-R]; i<end ;++i) {
            a[Depth-R] = i;
            __Foreach_cartesian<F,Depth,IndexArray,TI,R-1>::go(a, f, uppers, lowers);
        }
    }
};

template <typename F, size_t Depth, typename IndexArray, typename TI>
struct __Foreach_cartesian<F,Depth,IndexArray,TI,1> {
    static
    void go(IndexArray &a, F f, const IndexArray &uppers, const IndexArray &lowers) {
        TI end = uppers[Depth-1];
        for(TI i=lowers[Depth-1]; i<end ;++i) {
            a[Depth-1] = i;
            f(a);
        }
    }
};

template <size_t Depth, typename TI=int, typename F>
void foreach_cartesian(const std::array<TI,Depth> &_uppers, const std::array<TI,Depth> &_lowers, F f) {
    std::array<TI,Depth> a;
    __Foreach_cartesian<F,Depth,decltype(a), TI>::go(a, f, _uppers,_lowers);
}

template <size_t Depth, typename TI=int, typename F>
void foreach_cartesian(TI max, const std::array<TI,Depth> &lowers, F f) {
    std::array<TI,Depth> uppers;
    uppers.fill(max);

    foreach_cartesian(uppers, lowers, f);
}

template <size_t Depth, typename TI=int, typename F>
void foreach_cartesian(const std::array<TI,Depth> &uppers, F f) {
    foreach_cartesian(uppers, std::array<TI,Depth>{0,}, f);
}

template <size_t Depth, typename TI=int, typename F>
void foreach_cartesian(TI max, F f) {
    foreach_cartesian(max, std::array<TI,Depth>{0,}, f);
}

template <size_t Depth, typename TI=int, typename F>
void foreach_cartesian(TI max, TI min, F f) {
    std::array<TI,Depth> lowers;
    lowers.fill(min);

    foreach_cartesian(max, lowers, f);
}

template <size_t Depth, typename TI=int, typename F>
void foreach_cartesian(const std::array<TI,Depth> &uppers, TI min, F f) {
    std::array<TI,Depth> lowers;
    lowers.fill(min);

    foreach_cartesian(uppers, lowers, f);
}

/*//////////////////////////////////////////////////////////////////////////////////////////////
    tuple utils
*/
//iseq<1,2,3...>
template <int...Ti>
using iseq = std::integer_sequence<int,Ti...>;

template<typename...T>
struct iseq_cat;

template<int...Ti, int...Ui>
struct iseq_cat<iseq<Ti...>, iseq<Ui...>> {
    using type = iseq<Ti...,Ui...>;
};
template<typename T,typename U,typename...Rs>
struct iseq_cat<T,U,Rs...> {
    using type = typename iseq_cat<typename iseq_cat<T,U>::type, Rs...>::type;
};
template<int...Ti>
struct iseq_cat<iseq<Ti...>> {
    using type = iseq<Ti...>;
};

template<>
struct iseq_cat<> {
    using type = iseq<>;
};

template<typename...T>
using iseq_cat_t = typename iseq_cat<T...>::type;



template<typename T, int...I>
struct iseq_add;
template<int...Ti, int...Ui>
struct iseq_add<iseq<Ti...>, Ui...> {
    using type = iseq<Ti...,Ui...>;
};
template<typename T, int...U>
using iseq_add_t = typename iseq_add<T,U...>::type;



/*---------------------------------------------------------------------------------------------
    iseq_indexOf
*/
namespace __vane_helper__ {
    template <int X, typename T, bool _assert_=true>
    struct __iseq_indexOf;

    template <bool _assert_, int X, int...I>
    struct __iseq_indexOf<X,iseq<X,I...>, _assert_> : std::integral_constant<int, 0> { };

    template <bool _assert_, int X, int Y, int...I>
    struct __iseq_indexOf<X,iseq<Y,I...>, _assert_>
        : std::integral_constant<int, (0<=__iseq_indexOf<X,iseq<I...>,_assert_>::value)
                                    ?  1 + __iseq_indexOf<X,iseq<I...>,_assert_>::value
                                    : -1
                                > { };
    template <int X>
    struct __iseq_indexOf<X,iseq<>, false> : std::integral_constant<int, -1> { };
}

template <int X, typename T, bool _assert_=true>
constexpr int iseq_indexOf = __vane_helper__::__iseq_indexOf<X,T,_assert_>::value; 

/*---------------------------------------------------------------------------------------------
    iseq_at<3,iseq<0,1,2,3>>
*/
template <size_t I, typename Seq>
struct __iseq_at;

template <size_t I, int S, int...Ss>
struct __iseq_at<I, iseq<S,Ss...>>
    : std::integral_constant<int, __iseq_at<I-1, iseq<Ss...>>::value> { };

template <int S, int...Ss>
struct __iseq_at<0, iseq<S,Ss...>>
    : std::integral_constant<int, S> { };

template <size_t I, typename Seq>
constexpr int iseq_at = __iseq_at<I,Seq>::value;


/*---------------------------------------------------------------------------------------------
    __T_value_v<T,int,3>
*/
template<typename T, typename V, V val, typename=void_v<V>>
struct __T_value : std::integral_constant<V,val> {};

template< typename T, typename V, V val>
struct __T_value<T,V,val, void_v<V,T::value>> : std::integral_constant<decltype(T::value),T::value> {};

template<typename T, typename V, V val>
constexpr auto __T_value_v = __T_value<T,V,val>::value;

/*---------------------------------------------------------------------------------------------
    __T_map_t<T, void>
*/
template<typename T, typename X, typename=__void_t<X>>
struct __T_map { using map = X; };

template< typename T, typename X>
struct __T_map<T,X,__void_t<typename T::map>> { using map = typename T::map; };

template<typename T, typename X>
using __T_map_t = typename __T_map<T,X>::map;


/*---------------------------------------------------------------------------------------------
    __T_type_t<T, void>
*/
template<typename T, typename X, typename=__void_t<X>>
struct __T_type { using map = X; };

template< typename T, typename X>
struct __T_type<T,X,__void_t<typename T::map>> { using map = typename T::map; };

template<typename T, typename X>
using __T_type_t = typename __T_type<T,X>::map;


/*---------------------------------------------------------------------------------------------
    iseq_map<ISeq, filter, params...>
*/
template<typename ISeq, template<int SI, size_t I, typename _ISeq, typename...Params> class Filter, typename Indexer=std::make_index_sequence<ISeq::size()>, typename...FilterParam>
struct __iseq_map;

template<typename ISeq, template<int SI, size_t I, typename _ISeq, typename...Params> class Filter, typename...FilterParam, size_t...I>
struct __iseq_map<ISeq, Filter, std::index_sequence<I...>, FilterParam...> {
    using type = iseq_cat_t<
        typename std::conditional<
            __T_value_v< Filter<iseq_at<I,ISeq>,I,ISeq,FilterParam...>,  bool, true>,
            __T_map_t < Filter<iseq_at<I,ISeq>,I,ISeq,FilterParam...>,  iseq<iseq_at<I,ISeq>> >,
            iseq<>
        >::type...
    >;
    using complement = iseq_cat_t<
        typename std::conditional<
            __T_value_v< Filter<iseq_at<I,ISeq>,I,ISeq,FilterParam...>,  bool, true>,
            iseq<>,
            __T_map_t < Filter<iseq_at<I,ISeq>,I,ISeq,FilterParam...>,  iseq<iseq_at<I,ISeq>> >
        >::type...
    >;
};

template<typename ISeq, template<int SI, size_t I, typename _ISeq, typename...Params> class Filter, typename...FilterParam>
using iseq_map = typename __iseq_map<ISeq, Filter, std::make_index_sequence<ISeq::size()>, FilterParam...>::type;

template<typename ISeq, template<int SI, size_t I, typename _ISeq, typename...Params> class Filter, typename...FilterParam>
using iseq_map2 = __iseq_map<ISeq, Filter, std::make_index_sequence<ISeq::size()>, FilterParam...>;

/*---------------------------------------------------------------------------------------------
    iseq_andAll<1,2,3>
    iseq_orAll<1,2,3>
*/
template<typename T>
struct __iseq_andAll;

template<int...I>
struct __iseq_andAll<iseq<I...>> 
    : std::__and_<  std::integral_constant<bool, (bool)I>... > { };

template<typename T>
constexpr bool iseq_andAll = __iseq_andAll<T>::value;


template<typename T>
struct __iseq_orAll;

template<int...I>
struct __iseq_orAll<iseq<I...>> 
    : std::__or_<  std::integral_constant<bool, (bool)I>... > { };

template<typename T>
constexpr bool iseq_orAll = __iseq_orAll<T>::value;


/*---------------------------------------------------------------------------------------------
    iseq_sum<1,2,3>
*/
template<typename T>
struct iseq_sum;

template<int I, int...J>
struct iseq_sum<iseq<I,J...>> 
    : std::integral_constant<int, I + iseq_sum<iseq<J...>>::value > { };

template<int I>
struct iseq_sum<iseq<I>> 
    : std::integral_constant<int, I> { };

template<typename T>
constexpr int iseq_sum_v = iseq_sum<T>::value;

/*---------------------------------------------------------------------------------------------
    iseq_max<1,2,3>::value
    iseq_min<1,2,3>::value
*/
template<typename T>
struct iseq_max;

template<int I, int...J>
struct iseq_max<iseq<I,J...>> 
    : std::integral_constant<int,  (I > iseq_max<iseq<J...>>::value) ? I : iseq_max<iseq<J...>>::value> { };

template<int I>
struct iseq_max<iseq<I>> 
    : std::integral_constant<int, I> { };
template<typename T>
constexpr int iseq_max_v = iseq_max<T>::value;

template<typename T>
struct iseq_min;

template<int I, int...J>
struct iseq_min<iseq<I,J...>> 
    : std::integral_constant<int,  (I < iseq_min<iseq<J...>>::value) ? I : iseq_min<iseq<J...>>::value> { };

template<int I>
struct iseq_min<iseq<I>> 
    : std::integral_constant<int, I> { };

template<typename T>
constexpr int iseq_min_v = iseq_min<T>::value;


/*---------------------------------------------------------------------------------------------
    iseq_n<cnt, value>
*/
template<int Value, typename T>
struct __iseq_n;

template<int Value, size_t...I>
struct __iseq_n<Value, std::index_sequence<I...>> {
    using type = iseq<(((int)I-(int)I)+Value)... >;
};

template<int N, int Value>
using iseq_n = typename __iseq_n<Value, std::make_index_sequence<N>>::type;



/*---------------------------------------------------------------------------------------------
    tuple_index<T, tuple<...>, _assert_=true>
*/
template <class T, class typelist, bool _assert_=true>
struct ___tuple_index;

template <bool _assert_, class T, class...Types>
struct ___tuple_index<T, std::tuple<T, Types...>, _assert_> {
    enum { value = 0 };
};

template <class T>
struct ___tuple_index<T, std::tuple<>, false> {
    enum { value = -1 };
};

template <class T, class U, class... Types, bool _assert_>
struct ___tuple_index<T, std::tuple<U, Types...>, _assert_> :
    std::conditional< 
        0<=___tuple_index<T, std::tuple<Types...>, _assert_>::value,
        std::integral_constant<int,1+___tuple_index<T, std::tuple<Types...>, _assert_>::value>,
        std::integral_constant<int,-1>
    >::type
{ };

template<typename T,typename List, bool _assert_=true, int _default_=-1>
struct _tuple_index {
    enum { _value = ___tuple_index<T,List, _assert_>::value };
    static constexpr int
        value = std::conditional_t<
            _value >= 0,
            std::integral_constant<int,_value>,
            std::integral_constant<int,_default_>
        >::value;
};

template<typename T,typename List, bool _assert_=true, int _default_=-1>
constexpr int tuple_index = _tuple_index<T,List, _assert_,_default_>::value;


/*---------------------------------------------------------------------------------------------
    tuple_count_v
*/
template <typename X, typename T>
struct __tuple_count;

template <typename X>
struct __tuple_count<X, std::tuple<>> : std::integral_constant<int, 0> { };

template <typename X, typename T, typename...U>
struct __tuple_count<X, std::tuple<T,U...>>
    : std::integral_constant<int, (std::is_same<T,X>::value ? 1 : 0) + __tuple_count<X,std::tuple<U...>>::value > { };

template <typename X, typename Tuple>
constexpr int tuple_count_v = __tuple_count<X,Tuple>::value;

/*---------------------------------------------------------------------------------------------
    tuple_n<N,T>
*/
namespace __vane_helper__ {
    template <typename T, typename U>
        struct __tuple_n;
    template <typename T, size_t...I>
        struct __tuple_n<T, std::integer_sequence<long unsigned,I...>> {
            using type = std::tuple< typename std::conditional<(I>=0),T,void>::type...   >;
        };
};

template <int N, typename T>
using tuple_n = typename __vane_helper__::__tuple_n<T, std::make_index_sequence<N>>::type;

/*---------------------------------------------------------------------------------------------
    tuple_cat_t<tuple<...>, tuple<...>, tuple<...>>
    tuple_cat_t<tuple<...> >
*/
template<typename...T>
struct __tuple_cat_helper;

template<typename...T>
struct __tuple_cat_helper<std::tuple<T...>> {
    using type = std::tuple<T...>;
};

template<typename...Ts, typename...Us>
struct __tuple_cat_helper<std::tuple<Ts...>, std::tuple<Us...>> {
    using type = std::tuple<Ts...,Us...>;
};

template<typename T, typename U, typename...Rest>
struct __tuple_cat_helper<T,U,Rest...> {
    using type = typename __tuple_cat_helper<typename __tuple_cat_helper<T,U>::type, Rest...>::type;
};

template<>
struct __tuple_cat_helper<> {
    using type = std::tuple<>;
};

template<typename...T>
    using tuple_cat_t = typename __tuple_cat_helper<T...>::type;

template<typename Set>
struct __tuple_catInner;

template<typename...T>
struct __tuple_catInner<std::tuple<T...>> {
    using type = tuple_cat_t<T...>;
};

template <typename...T>
using tuple_catInner = typename __tuple_catInner<T...>::type;

template <typename...T>
struct tuple_add;

template <typename...T, typename...U>
struct tuple_add<std::tuple<T...>,U...> {
    using type = tuple_cat_t<std::tuple<T...>, std::tuple<U>...>;
};

template <typename...T>
using tuple_add_t = typename tuple_add<T...>::type;


/*---------------------------------------------------------------------------------------------
    tuple_uniq_t
*/
template <typename T, typename Sum=std::tuple<>>
struct __tuple_uniq;

template <typename...S>
struct __tuple_uniq<std::tuple<>,std::tuple<S...>> {
    using type = std::tuple<S...>;
};

template <typename T,typename...Ts, typename...S>
struct __tuple_uniq<std::tuple<T,Ts...>,std::tuple<S...>> {
    using type = typename __tuple_uniq<
        std::tuple<Ts...>,
        typename std::conditional<
            (0>tuple_index<T,std::tuple<S...>,false>), std::tuple<S...,T>, std::tuple<S...>
        >::type
    >::type;
};

template <typename T>
using tuple_uniq_t = typename __tuple_uniq<T>::type;



/*---------------------------------------------------------------------------------------------
    type_map
*/
template<typename Tuple, template<typename,size_t,typename,typename...> class Filter, typename Seq=std::make_index_sequence<std::tuple_size<Tuple>::value>, typename...FilterParam>
struct __type_map;

template<typename Tuple, template<typename,size_t,typename,typename...> class Filter, typename...FilterParam, size_t...I>
struct __type_map<Tuple, Filter, std::index_sequence<I...>, FilterParam...> {
    using type = tuple_cat_t<
        typename std::conditional<
            __T_value_v<Filter<std::tuple_element_t<I,Tuple>,I,Tuple,FilterParam...>,  bool, true>,
            __T_map_t  <Filter<std::tuple_element_t<I,Tuple>,I,Tuple,FilterParam...>,  std::tuple<typename std::tuple_element<I,Tuple>::type>>,
            std::tuple<>
        >::type...
    >;
    using complement = tuple_cat_t<
        typename std::conditional<
            __T_value_v<Filter<std::tuple_element_t<I,Tuple>,I,Tuple,FilterParam...>,  bool, true>,
            std::tuple<>,
            std::tuple<std::tuple_element_t<I,Tuple>>
        >::type...
    >;
};

template<typename Tuple, template<typename,size_t,typename,typename...> class Filter,typename...FilterParam>
using type_map = typename __type_map<Tuple,Filter,std::make_index_sequence<std::tuple_size<Tuple>::value>,FilterParam...>::type;

template<typename Tuple, template<typename,size_t,typename,typename...> class Filter,typename...FilterParam>
using type_map2 = __type_map<Tuple,Filter,std::make_index_sequence<std::tuple_size<Tuple>::value>,FilterParam...>;


/*---------------------------------------------------------------------------------------------
    type_mapv
*/
template<typename Tuple, template<typename,size_t,typename,typename...> class Mapper, typename Seq=std::make_index_sequence<std::tuple_size<Tuple>::value>, typename...FilterParam>
struct __type_mapv;

template<typename Tuple, template<typename,size_t,typename,typename...> class Mapper, typename...FilterParam, size_t...I>
struct __type_mapv<Tuple, Mapper, std::index_sequence<I...>, FilterParam...> {
    using type = iseq_cat_t<
        __T_map_t  <
            Mapper<std::tuple_element_t<I,Tuple>,I,Tuple,FilterParam...>,
            iseq< __T_value_v< Mapper< std::tuple_element_t<I,Tuple>,I,Tuple,FilterParam...>, int, 0> >
        >...
    >;
};

template<typename Tuple, template<typename,size_t,typename,typename...> class Mapper,typename...FilterParam>
using type_mapv = typename __type_mapv<Tuple,Mapper,std::make_index_sequence<std::tuple_size<Tuple>::value>,FilterParam...>::type;


/*---------------------------------------------------------------------------------------------
    tuple_all_of
    tuple_any_of
*/
template<typename T,template<typename,size_t,typename,typename...>class Filter, typename...Param>
struct __tuple_all_of;

template<template<typename,size_t,typename,typename...>class Filter, typename...T, typename...Param>
struct __tuple_all_of<std::tuple<T...>,Filter,Param...>
    : std::integral_constant<bool, sizeof...(T)==std::tuple_size< type_map< std::tuple<T...>, Filter, Param... > >::value > { };

template<typename T,template<typename,size_t,typename,typename...>class Filter, typename...Param>
constexpr bool tuple_all_of = __tuple_all_of<T,Filter,Param...>::value;

template<typename T,template<typename,size_t,typename,typename...>class Filter, typename...Param>
struct __tuple_any_of;

template<template<typename,size_t,typename,typename...>class Filter, typename...T, typename...Param>
struct __tuple_any_of<std::tuple<T...>,Filter,Param...>
    : std::integral_constant<bool, !!std::tuple_size< type_map< std::tuple<T...>, Filter, Param... > >::value > { };

template<typename T,template<typename,size_t,typename,typename...>class Filter, typename...Param>
constexpr bool tuple_any_of = __tuple_any_of<T,Filter,Param...>::value;


/*---------------------------------------------------------------------------------------------
    iseq_any_of
*/
template<typename S,template<int,size_t,typename,typename...>class Filter, typename...Param>
struct __iseq_any_of;

template<template<int,size_t,typename,typename...>class Filter, int...I, typename...Param>
struct __iseq_any_of<iseq<I...>,Filter,Param...>
    : std::integral_constant<bool, !!iseq_map<iseq<I...>, Filter, Param...>::size() > { };

template<typename S,template<int,size_t,typename,typename...>class Filter, typename...Param>
constexpr bool iseq_any_of = __iseq_any_of<S,Filter,Param...>::value;

template<typename S,template<int,size_t,typename,typename...>class Filter, typename...Param>
struct __iseq_all_of;

template<template<int,size_t,typename,typename...>class Filter, int...I, typename...Param>
struct __iseq_all_of<iseq<I...>,Filter,Param...>
    : std::integral_constant<bool, sizeof...(I)==iseq_map< iseq<I...>, Filter, Param... >::size() > { };

template<typename S,template<int,size_t,typename,typename...>class Filter, typename...Param>
constexpr bool iseq_all_of = __iseq_all_of<S,Filter,Param...>::value;


/*---------------------------------------------------------------------------------------------
*/
namespace iseq_filters
{
    template<int Si, size_t I, typename ISeq, typename Tuples>
    struct map_tuple_size
        { using map = iseq< std::tuple_size<std::tuple_element_t<I,Tuples>>::value >; };

    template<int SI, size_t I, typename ISeq>
    struct map_nonZero_into_index   { using map = iseq< SI ? I : 0>; };


    template<int SI, size_t I, typename ISeq>
    struct filter_nonZero_into_index : std::integral_constant<bool, !!SI> { using map = iseq<I>; };

    template<int SI, size_t I, typename ISeq, typename V>
    struct filter_value_into_index : std::integral_constant<bool, SI==V::value> { using map = iseq<I>; };

    template<int SI, size_t I, typename ISeq>
    struct filter_ge0_into_index : std::integral_constant<bool, SI>=0 > { using map = iseq<I>; };

    template<int SI, size_t I, typename ISeq>
    struct map_not
        { using map = iseq< !SI >; };

    template<int SI, size_t I, typename ISeq>
    struct map_inverse
        { using map = iseq< iseq_indexOf<I,ISeq> >; };

    template<typename EI, size_t I, typename Tuple, typename Ref, typename _assert_=std::false_type, typename _default_=std::integral_constant<int,-1>>
    struct map_index {
        using map = iseq< tuple_index<EI,Ref,_assert_::value, _default_::value> >;
    };
}


    template<typename Tuples>
    using tuple_sizes = iseq_map<std::make_integer_sequence<int,std::tuple_size<Tuples>::value>, iseq_filters::map_tuple_size, Tuples>;

    template<typename ISeq>
    using iseq_inverse = iseq_map<ISeq, iseq_filters::map_inverse>;


    template<typename ISeq, int D>
    struct iseq_inc;

    template<int...I, int D>
    struct iseq_inc<iseq<I...>,D> {
        using type = iseq<(I+D)...>;
    };

    template<typename ISeq, int S>
    struct iseq_scale;

    template<int...I, int S>
    struct iseq_scale<iseq<I...>,S> {
        using type = iseq<(I*S)...>;
    };

    template<typename SeqA, typename SeqB>
    struct iseq_plus;

    template<int...I, int...J>
    struct iseq_plus<iseq<I...>,iseq<J...>> {
        using type = iseq<(I+J)...>;
    };


    template<typename SeqA, typename SeqB>
    struct iseq_mul;

    template<int...I, int...J>
    struct iseq_mul<iseq<I...>,iseq<J...>> {
        using type = iseq<(I*J)...>;
    };


    template<typename SeqA, typename SeqB>
    struct iseq_div;

    template<int...I, int...J>
    struct iseq_div<iseq<I...>,iseq<J...>> {
        using type = iseq<(I/J)...>;
    };


    template<typename ISeq, typename G=iseq<0>, int S=0>
    struct _ISeq_partial_sum;

    template<int I0, int...I, typename G>
    struct _ISeq_partial_sum<iseq<I0, I...>, G> {
        using next = _ISeq_partial_sum<iseq<I...>, iseq_add_t<G, iseq_at<G::size()-1,G>+I0>>;
        using type = typename next::type;
    };

    template<typename G>
    struct _ISeq_partial_sum<iseq<>, G> {
        using type = G;
    };




/*---------------------------------------------------------------------------------------------
    predefined type_filters:
*/
namespace type_filters {
    template<typename T, size_t I, typename Tuple, typename D>
    struct is_base_of: std::is_base_of<T,D> { };

    template<typename T, size_t I, typename Tuple, typename B>
    struct is_sub_of : std::is_base_of<B,T> { };

    template<typename T, size_t I, typename Tuple, typename X>
    struct is_same: std::is_same<T,X> { };

    template<typename T, size_t I, typename Tuple, typename X>
    struct is_not_same: std::integral_constant<bool, !std::is_same<T,X>::value > { };

    template<typename T, size_t I, typename Tuple, typename D>
    struct basetype_is_base_of: vane::basetype_is_base_of<T,D> { };

    template<typename T, size_t I, typename Tuple, typename B>
    struct basetype_is_derived_from: vane::basetype_is_base_of<B,T> { };

    template<typename T, size_t I, typename Tuple>
    struct is_pointer: std::is_pointer<T> { };

    template <typename T, size_t I, typename Tuple, typename List>
    struct is_topbase_of
        : std::integral_constant<bool, ! tuple_any_of<type_map<List, type_filters::is_not_same, T>, type_filters::is_base_of,T>> { };
}

template<typename T, typename D>
struct __is_base_of_all;

template<typename T, typename...D>
struct __is_base_of_all<T,std::tuple<D...>>
    : std::integral_constant<bool, tuple_all_of<std::tuple<D...>,type_filters::is_sub_of, T> > { };

template<typename T>
struct __is_base_of_all<T,std::tuple<>>;

template<typename T, typename D>
constexpr bool is_base_of_all = __is_base_of_all<T,D>::value;

//-------------------------------------------------------------------------
template<typename T, typename B>
struct __is_derived_of_all;

template<typename T>
struct __is_derived_of_all<T,std::tuple<>>;

template<typename T, typename...B>
struct __is_derived_of_all<T,std::tuple<B...>>
    : std::integral_constant<bool, tuple_all_of<std::tuple<B...>,type_filters::is_base_of, T> > { };

template<typename T, typename B>
constexpr bool is_derived_of_all = __is_derived_of_all<T,B>::value;

namespace type_filters {
    template<typename T, size_t I, typename, typename TypeList>
    struct is_base_of_all: vane::__is_base_of_all<T, TypeList> { };

    template<typename T, size_t I, typename, typename TypeList>
    struct is_derived_of_all: std::integral_constant<bool, vane::is_derived_of_all<T, TypeList> > { };

    template<typename T, size_t I, typename, typename List>
    struct is_base_of_all_at : 
        std::integral_constant<bool, vane::is_base_of_all<T, std::tuple_element_t<I,List>>> { };
}


/*----------------------------------------------------------------
    remove_const<T>
*/
    template<typename T>
    struct remove_const  {
        using type =
            std::conditional_t  < std::is_pointer<T>::value,            std::add_pointer_t<std::remove_const_t<std::remove_pointer_t<T>>>,
            std::conditional_t  < std::is_lvalue_reference<T>::value,   std::remove_const_t<std::remove_reference_t<T>> &,
            std::conditional_t  < std::is_rvalue_reference<T>::value,   std::remove_const_t<std::remove_reference_t<T>> &&,
                                                                        std::remove_const_t<T>
            >>>;
    };
    template<typename T>
    using remove_const_t = typename remove_const<T>::type;


/*----------------------------------------------------------------
    type_filters
*/
namespace type_filters {
    template<typename T, size_t I, typename Tuple, typename N>
    struct skipN : std::integral_constant<bool, (int(I)>= N::value) > { };

    template<typename T, size_t I, typename Tuple>
    struct map_tuple_wrapped { using map = std::tuple< std::tuple<T> >; };

    template<typename X, size_t I, typename T>
    struct map_add_pointer
        { using map = std::tuple<std::add_pointer_t<X>>; };

    template<typename Tuple, size_t I, typename>
    struct map_add_pointers
        { using map = std::tuple< type_map<Tuple, map_add_pointer> >;   };

    template<typename X, size_t I, typename T>
    struct map_add_reference
        { using map = std::tuple<X&>; };

    template<typename X, size_t I, typename T>
    struct map_add_rvalue_reference
        { using map = std::tuple<std::add_rvalue_reference_t<X>>; };

    template<typename X, size_t I, typename T>
    struct map_remove_pointer
        { using map = std::tuple<std::remove_pointer_t<X>>; };

    template<typename X, size_t I, typename T>
    struct map_remove_reference
        { using map = std::tuple<std::remove_reference_t<X>>; };

    template<typename X, size_t I, typename T>
    struct map_remove_const
        { using map = std::tuple<remove_const_t<X>>; };

    template<typename X, size_t I, typename T>
    struct map_remove_pointer_or_reverence {
        using map = std::tuple< remove_pointer_or_reference_t<X> >;
    };

    template <typename X, size_t I, typename T, typename S, typename E>
    struct is_index_in_range;

    template <typename X, size_t I, typename T, int S, int E>
    struct is_index_in_range<X,I,T,std::integral_constant<int,S>, std::integral_constant<int,E>>
        : std::integral_constant<bool, (S<=I) && (I<E)> { };

    template<typename X, size_t I, typename>
    struct map_tuple_size {
        using map = iseq<std::tuple_size<X>::value>; 
    };

    template<typename T, size_t, typename, typename List, typename inclusive=std::false_type>
    struct map_subclasses
    {
        using map = std::tuple<type_map<
                std::conditional_t<inclusive::value,
                    List,
                    type_map<List, type_filters::is_not_same, T>
                >,
                type_filters::is_sub_of,
                T
            >>;
    };
}

template<typename T, int N>
using tuple_skip_n = type_map<T, type_filters::skipN, std::integral_constant<int, N>>;

//add_xxx ///////////////////////////////////////////////////////////////////////////
template<typename Tuple>
using add_pointers = type_map<Tuple, type_filters::map_add_pointer>;

template<typename TT>
using add_pointers_nested = type_map<TT, type_filters::map_add_pointers >;


template<typename Tuple>
using add_references = type_map<Tuple, type_filters::map_add_reference>;

template<typename Tuple>
using add_rvalue_references = type_map<Tuple, type_filters::map_add_rvalue_reference>;

//remove_xxx
template<typename Tuple>
using remove_pointers = type_map<Tuple, type_filters::map_remove_pointer>;

template<typename Tuple>
using remove_references = type_map<Tuple, type_filters::map_remove_reference>;

template<typename Tuple>
using remove_consts = type_map<Tuple, type_filters::map_remove_const>;

template<typename Tuple>
using remove_pointers_or_references = type_map<Tuple, type_filters::map_remove_pointer_or_reverence>;


/*---------------------------------------------------------------------------------------------
tuple_elements
*/
template<typename Seq, typename Tuple>
struct __helper_tuple_elements;

template<size_t...I, typename Tuple>
struct __helper_tuple_elements<std::index_sequence<I...>, Tuple> {
    using type = std::tuple<typename std::tuple_element<I, Tuple>::type...>;
};
template<int...I, typename Tuple>
struct __helper_tuple_elements<std::integer_sequence<int,I...>, Tuple> {
    using type = std::tuple<typename std::tuple_element<I, Tuple>::type...>;
};

template<typename Seq, typename Tuple>
using tuple_elements = typename __helper_tuple_elements<Seq,Tuple>::type;



/*---------------------------------------------------------------------------------------------
    tuple_sizes<Domains>
*/
template<typename Tuples>
using tuples_sizes = type_mapv<Tuples, type_filters::map_tuple_size>;

/*---------------------------------------------------------------------------------------------
    linear_sizeof_tuples<Domains>
*/
    template<typename Tuples>
    constexpr auto linear_sizeof_tuples = iseq_sum_v<tuples_sizes<Tuples>>;


/*--------------------------------------------------------------------------------------
    make_array(...)
*/
template<typename...T>
constexpr
auto make_array(T...args) {
    using _T = typename std::common_type<typename std::__decay_and_strip<T>::__type...>::type;
    return std::array<_T,sizeof...(T)>{std::forward<_T>(args)...};
}

/*--------------------------------------------------------------------------------------
    make_array<iseq>
*/
template<typename ET=int, ET...I>
constexpr auto __helper_make_array(std::integer_sequence<ET,I...>) {
    return std::array<ET,sizeof...(I)>{ I... };
}

template<typename ISeq>
constexpr auto make_array() {
    return __helper_make_array(ISeq());
}

/*--------------------------------------------------------------------------------------
    make_array<ET,iseq>
*/
template<typename ET, typename IT=int, IT...I>
constexpr auto __helper_make_array(ET, std::integer_sequence<IT,I...>) {
    return std::array<ET,sizeof...(I)>{ ET(I)... };
}

template<typename ET, typename ISeq>
constexpr auto make_array() {
    return __helper_make_array<ET>(ET(), ISeq());
}


/*-----------------------------------------------------------
    stacked_allocator
    gstack
*/

//exception
struct invalid_stack_marker : std::runtime_error {
    invalid_stack_marker(const char *m="invalid stack marker") : runtime_error(m) {}

    static void __throw() __vane_noinline {
        throw invalid_stack_marker();
    }
};


template <class __T>
inline 
void __destroy_at(__T *p) {
    p->~__T();
}



template <class __T>
inline
std::enable_if_t<std::is_trivially_destructible<__T>::value>
__destroy(__T *first, __T *last) { }

template <class __T>
inline __vane_noinline
std::enable_if_t<!std::is_trivially_destructible<__T>::value>
__destroy(__T *first, __T *last) {
    for(; first < last ;++first)
        __destroy_at( first );
}



template<typename __T, typename=void>
struct __sa_destroy_at
{
    void operator()(std::remove_extent_t<__T> *p) const {
        p->~__T();
    }
};

template<typename __T>
struct __sa_destroy_at<__T, std::enable_if_t<std::is_trivially_destructible<std::remove_extent_t<__T>>::value>>
{
    void operator()(std::remove_extent_t<__T> *p) const { }
};


template<typename __T>
struct __sa_destroy_array
{
    void operator()(std::remove_extent_t<__T> *p) const {
        assert( p );
        assert( ((unsigned*)p)[-1] );

        __destroy(p, p + ((unsigned*)p)[-1]);
    }
};



template<size_t __I=0, typename __UT=unsigned int>
class stacked_allocator
{
    stacked_allocator(const stacked_allocator&) = delete;
    stacked_allocator(stacked_allocator&&) = delete;
    stacked_allocator &operator=(const stacked_allocator&) = delete;
    stacked_allocator &operator=(stacked_allocator&&) = delete;
protected:
    using slot = std::vector<__UT>;

public:
    using unit_type = __UT;
    using stack_mark = std::array<unsigned,2>;


    stacked_allocator() {
        _slots.resize(1);
        _cur = _slots.begin();
    }

    void *allocate(unsigned size)
    {
        if( size == 0 ) return nullptr;

        size = (size + sizeof(__UT)-1) / sizeof(__UT);
        if( size > _cur->capacity() - _cur->size() ) {

            unsigned sum = _extra_size;
            _extra_size = 0;
            for(auto i=_cur+1; i < _slots.end() ;++i)
                sum += i->capacity();
            sum = sum < size ? size : sum;

            _slots.resize( _cur - _slots.begin() + (_cur->size() ? 2 : 1));
            _cur = _slots.end() - 1;

            _cur->resize(0);
            _cur->reserve(sum);
        }

        auto old_top = _cur->size();
        _cur->resize( old_top + size );

        return &(*_cur)[old_top];
    }

    void *allocate_n(unsigned n, unsigned size)
    {
        auto size_data = n*size;
        if( size_data==0 ) return nullptr;

        size_data = (size_data + sizeof(__UT)-1) / sizeof(__UT) * sizeof(__UT);
        constexpr auto size_n = (sizeof(n) + sizeof(__UT)-1) / sizeof(__UT) * sizeof(__UT);

        char *data = (char*)allocate( size_n + size_data ) + size_n;
        ((unsigned*)data)[-1] = n;

        return data;
    }

    void reclaim( stack_mark top )
    {
        const auto &to_cur = top[0];
        const auto &to_top = top[1];
        const auto icur = _cur - _slots.begin();

        if( to_cur > icur ) {
            assert( to_cur <= icur );
            invalid_stack_marker::__throw();
        }
        else if( to_cur < icur ) {
            _cur = _slots.begin() + to_cur;
        }

        if( to_top > _cur->size() ) {
            assert( to_top <= _cur->size() );
            invalid_stack_marker::__throw();
        }

        if ( to_top==0 && _cur != _slots.end()-1 ){
            _extra_size += _cur->capacity();
            slot().swap(*_cur);
        }
        else
            _cur->resize(to_top);
    }

    stack_mark top() {
        assert( _cur - _slots.begin() <= std::numeric_limits<unsigned>::max() );
        assert( _cur->size()          <= std::numeric_limits<unsigned>::max() );
        return stack_mark{(unsigned)(_cur - _slots.begin()), (unsigned)_cur->size()};
    }

    void reserve(unsigned size) {
        auto mark = top();
        allocate(size);
        reclaim( mark );
    }
    
public:
    template<typename T, typename...Args>
    typename std::enable_if_t<!std::is_array<T>::value || std::extent<T>::value, std::unique_ptr<T,__sa_destroy_at<T>>>
    alloc(Args&&...args) {
        return std::unique_ptr<T,__sa_destroy_at<T>>(new(allocate(sizeof(T))) T{std::forward<Args>(args)...});
    }

    template<typename T>
    typename std::enable_if_t<!std::is_trivially_destructible<std::remove_extent_t<T>>::value && std::is_array<T>::value && std::extent<T>::value==0, std::unique_ptr<T,__sa_destroy_array<T>>>
    alloc(unsigned n) {
        using TT = std::remove_extent_t<T>;
        return std::unique_ptr<T,__sa_destroy_array<T>>(new(allocate_n(n, sizeof(TT))) TT[n]);
    }

    template<typename T>
    typename std::enable_if_t<std::is_trivially_destructible<std::remove_extent_t<T>>::value && std::is_array<T>::value && std::extent<T>::value==0, std::unique_ptr<T,__sa_destroy_at<T>>>
    alloc(unsigned n) {
        using TT = std::remove_extent_t<T>;
        return std::unique_ptr<T,__sa_destroy_at<T>>(new(allocate(n*sizeof(TT))) TT[n]);
    }


    template<typename T, typename...Args>
    typename std::enable_if_t<!std::is_trivially_destructible<std::remove_extent_t<T>>::value && std::is_array<T>::value && std::extent<T>::value==0, std::unique_ptr<T,__sa_destroy_array<T>>>
    make_array(Args&&...args) {
        using TT = std::remove_extent_t<T>;
        constexpr auto n = sizeof...(Args);
        return std::unique_ptr<T,__sa_destroy_array<T>>(new(allocate_n(n, sizeof(TT))) TT[n]{std::forward<Args>(args)...});
    }

    template<typename T, typename...Args>
    typename std::enable_if_t<std::is_trivially_destructible<std::remove_extent_t<T>>::value && std::is_array<T>::value && std::extent<T>::value==0, std::unique_ptr<T,__sa_destroy_at<T>>>
    make_array(Args&&...args) {
        using TT = std::remove_extent_t<T>;
        constexpr auto n = sizeof...(Args);
        return std::unique_ptr<T,__sa_destroy_at<T>>(new(allocate(n*sizeof(TT))) TT[n]{std::forward<Args>(args)...});
    }


    static stacked_allocator    &get_instance() { return __instance; };

protected:
    typename std::vector<slot>::iterator    _cur;
    std::vector<slot>                       _slots;

private:
    unsigned                    _extra_size = 0;
    static stacked_allocator    __instance;
};

template<size_t __I, typename __UT>
stacked_allocator<__I,__UT>
stacked_allocator<__I,__UT>::__instance;




////////////////////////////////////////////////////////////////////////////////////////////
template<size_t __I=0, typename __UT=unsigned int>
class gstack
{
    gstack(const gstack&) = delete;
    gstack(gstack&&) = delete;
    gstack &operator=(const gstack&) = delete;
    gstack &operator=(gstack&&) = delete;

public:
    gstack() : _mark( get_allocator().top() ) { }

    ~gstack() {
        get_allocator().reclaim( _mark );
    }

    void *allocate(unsigned size) {
        return get_allocator().allocate(size);
    }

    void reserve(unsigned size) {
        return get_allocator().reserve(size);
    }


    template<typename T, typename...Args>
    auto
    alloc(Args&&...args) {
        return get_allocator().template alloc<T>(std::forward<Args>(args)...);
    }

    template<typename T, typename...Args>
    auto
    make_array(Args&&...args) {
        return get_allocator().template make_array<T>(std::forward<Args>(args)...);
    }

    static auto &get_allocator() {
        return stacked_allocator<__I,__UT>::get_instance();
    }


protected:
    std::array<unsigned,2>  _mark;
};






//////////////////////////////////////////////////////////////////////////////////////////////////
template<typename __T, unsigned __I=1>
class gstack_allocator
{
public:
    using size_type         = size_t;
    using difference_type   = ptrdiff_t;
    using pointer           = __T*;
    using const_pointer     = const __T*;
    using reference         = __T&;
    using const_reference   = const __T&;
    using value_type        = __T;

    template<typename __T1>
    struct rebind {
        using other = gstack_allocator<__T1,__I>; 
    };


    gstack_allocator() noexcept : gstack_allocator(stacked_allocator<__I>::get_instance()) { }
    gstack_allocator(gstack<__I> &gs) noexcept : gstack_allocator(gs.get_allocator()) { }
    gstack_allocator(stacked_allocator<__I> &sa) noexcept : _allocator(&sa) {
        assert(_allocator);
    }
    template<typename __T1>
    gstack_allocator(const gstack_allocator<__T1,__I> &ga) noexcept : gstack_allocator(ga.get_allocator()) { }


    ~gstack_allocator() noexcept { }



    __T *allocate(size_t n, const void *p= 0)
    {
        (void)p;
        return static_cast<__T*>(get_allocator().allocate(n * sizeof(__T)));
    }

    void deallocate(__T *, size_t) noexcept { }


    auto &get_allocator() const noexcept {
        return *_allocator;
    }

protected:
    stacked_allocator<__I>   *_allocator;

}; //gstack_allocator



template<typename __T,unsigned __I>
inline
bool operator==(const gstack_allocator<__T,__I>&, const gstack_allocator<__T,__I>&)
{ return true; }

template<typename __T,unsigned __I>
inline 
bool operator!=(const gstack_allocator<__T,__I>&, const gstack_allocator<__T,__I>&)
{ return false; }




//gs_array //////////////////////////////////////////////////////////////////////////
template<typename __T, long __SIZE=-1, size_t __GSI=1>
struct gs_array
{
    static_assert( __SIZE >= 0, "");

    using value_type             = __T;
    using pointer                = __T*;
    using const_pointer          = const __T*;
    using reference              = __T&;
    using const_reference        = const __T&;
    using iterator               = pointer;
    using const_iterator         = const_pointer;
    using reverse_iterator       = std::reverse_iterator<iterator>;
    using const_reverse_iterator = std::reverse_iterator<const_iterator>;

    using size_type         = size_t;
    using difference_type   = ptrdiff_t;


    gs_array(const gs_array&) = delete;
    gs_array &operator=(const gs_array&) = delete;
    gs_array(gs_array &&a) = default;
    gs_array &operator=(gs_array&&) = default;


    gs_array()
        : _data( stacked_allocator<__GSI>::get_instance().template alloc<__T[]>(size()) )
    { }
    template<typename...Args>
    gs_array(Args&&...args)
        : _data( stacked_allocator<__GSI>::get_instance().template alloc<__T[]>(size(), std::forward<Args>(args)...) )
    { }

    gs_array(gstack_allocator<__T,__GSI> gsa)
        : _data( gsa.get_allocator().template alloc<__T[]>(size()) )
    { }

    template<typename...Args>
    gs_array(gstack_allocator<__T,__GSI> gsa, Args&&...args)
        : _data( gsa.get_allocator().template alloc<__T[]>(size(), std::forward<Args>(args)...) )
    { }

    gs_array(gstack<__GSI> &gs)
        : _data( gs.get_allocator().template alloc<__T[]>(size()) )
    { }
    template<typename...Args>
    gs_array(gstack<__GSI> &gs, Args&&...args)
        : _data( gs.get_allocator().template alloc<__T[]>(size(), std::forward<Args>(args)...) )
    { }



    reference
    operator[](size_type i) {
        return __SIZE ? _data[i] : *(value_type*)nullptr;
    }

    const_reference
    operator[](size_type i) const {
        return __SIZE ? _data[i] : *(value_type*)nullptr;
    }

    pointer
    data() {
        return __SIZE ? _data.get() : nullptr;
    }

    const_pointer
    data() const {
        return _data.get();
    }

    reference
    front() {
        return __SIZE ? _data[0] : *(value_type*)nullptr;
    }
    const_reference
    front() const {
        return __SIZE ? _data[0] : *(value_type*)nullptr;
    }

    reference
    back() {
        return __SIZE ? _data[__SIZE-1] : *(value_type*)nullptr;
    }
    const_reference
    back() const {
        return __SIZE ? _data[__SIZE-1] : *(value_type*)nullptr;
    }

    constexpr bool      empty() noexcept    { return __SIZE==0; }
    constexpr size_type size()  noexcept    { return __SIZE;    }

    iterator begin() noexcept   { return iterator(data()); }
    iterator end()   noexcept   { return iterator(data() + __SIZE); }

    reverse_iterator rbegin() noexcept      { return reverse_iterator(end()); }
    reverse_iterator rend()   noexcept      { return reverse_iterator(begin()); }

    const_iterator begin() const noexcept   { return const_iterator(data()); }
    const_iterator end()   const noexcept   { return const_iterator(data() + __SIZE); }

    const_iterator cbegin() const noexcept  { return const_iterator(data()); }
    const_iterator cend()   const noexcept  { return const_iterator(data() + __SIZE); }

    const_reverse_iterator rbegin() const noexcept  { return const_reverse_iterator(end()); }
    const_reverse_iterator rend()   const noexcept  { return const_reverse_iterator(begin()); }

    const_reverse_iterator crbegin() const noexcept { return const_reverse_iterator(end()); }
    const_reverse_iterator crend()   const noexcept { return const_reverse_iterator(begin()); }


    void fill(const value_type &v) {
        std::fill_n(begin(), size(), v);
    }


    operator std::array<__T,__SIZE>&() {
        return *(std::array<__T,__SIZE>*)data();
    }

    std::array<__T,__SIZE>  &as_array() const {
        return *reinterpret_cast<std::array<__T,__SIZE>*>(data());
    }


private:
    static auto fff() {
        gstack<>    gs;
        return gs.alloc<__T[]>(1);
    }
    using data_ptr = decltype(fff());

protected:
    data_ptr    _data;


    friend struct gs_array<__T,-1,__GSI>;
};


template<typename __T, size_t __SIZE>
inline bool 
operator==(const gs_array<__T, __SIZE> &a, const gs_array<__T, __SIZE> &b)
{ return std::equal(a.begin(), a.end(), b.begin()); }

template<typename __T, size_t __SIZE>
inline bool
operator!=(const gs_array<__T, __SIZE> &a, const gs_array<__T, __SIZE> &b)
{ return !(a == b); }

template<typename __T, size_t __SIZE>
inline bool
operator<(const gs_array<__T, __SIZE> &a, const gs_array<__T, __SIZE> &b) { 
    return std::lexicographical_compare(a.begin(), a.end(), b.begin(), b.end()); 
}

template<typename __T, size_t __SIZE>
inline bool
operator>(const gs_array<__T, __SIZE> &a, const gs_array<__T, __SIZE> &b)
{ return b < a; }

template<typename __T, size_t __SIZE>
inline bool
operator<=(const gs_array<__T, __SIZE> &a, const gs_array<__T, __SIZE> &b)
{ return !(a > b); }

template<typename __T, size_t __SIZE>
inline bool
operator>=(const gs_array<__T, __SIZE> &a, const gs_array<__T, __SIZE> &b)
{ return !(a < b); }



template<typename __T, size_t __GSI>
struct gs_array<__T,-1,__GSI>
{
    using value_type             = __T;
    using pointer                = __T*;
    using const_pointer          = const __T*;
    using reference              = __T&;
    using const_reference        = const __T&;
    using iterator               = pointer;
    using const_iterator         = const_pointer;
    using reverse_iterator       = std::reverse_iterator<iterator>;
    using const_reverse_iterator = std::reverse_iterator<const_iterator>;

    using size_type         = unsigned;
    using difference_type   = ptrdiff_t;


    gs_array(const gs_array&) = delete;
    gs_array &operator=(const gs_array&) = delete;

    template<long _SIZE>
    gs_array(gs_array<__T,_SIZE,__GSI> &&a) {
        //_size = 0;
        _data = std::move(a._data);
        _size = _SIZE<0 ? a._size : _SIZE;
    }
    template<long _SIZE>
    gs_array &operator=(gs_array<__T,_SIZE,__GSI> &&a) {
        //_size = 0;
        _data = std::move(a._data);
        _size = _SIZE<0 ? a._size : _SIZE;
        return *this;
    }


    gs_array() {
        _size = 0;
    }

    gs_array(unsigned size) {
        assert( size );
        _data = stacked_allocator<__GSI>::get_instance().template alloc<__T[]>(size);
        _size = size;
    }

    template<typename...Args>
    gs_array(unsigned size, Args&&...args) : _size(0) {
        assert( size );
        _data = stacked_allocator<__GSI>::get_instance().template alloc<__T[]>(size, std::forward<Args>(args)...);
        _size = size;
    }
    gs_array(unsigned size, gstack_allocator<__T,__GSI> &gsa) {
        assert( size );
        _data = gsa.get_allocator().template alloc<__T[]>(size);
        _size = size;
    }

    gs_array(gstack_allocator<__T,__GSI> &gsa, unsigned size) {
        assert( size );
        _data = gsa.get_allocator().template alloc<__T[]>(size);
        _size = size;
    }
    template<typename...Args>
    gs_array(gstack_allocator<__T,__GSI> &gsa, unsigned size, Args&&...args) : _size(0) {
        assert( size );
        _data = gsa.get_allocator().template alloc<__T[]>(size, std::forward<Args>(args)...);
        _size = size;
    }

    gs_array(unsigned size, gstack<__GSI> &gs) {
        assert( size );
        _data = gs.get_allocator().template alloc<__T[]>(size);
        _size = size;
    }

    gs_array(gstack<__GSI> &gs, unsigned size) {
        assert( size );
        _data = gs.get_allocator().template alloc<__T[]>(size);
        _size = size;
    }
    template<typename...Args>
    gs_array(gstack<__GSI> &gs, unsigned size, Args&&...args) : _size(0) {
        assert( size );
        _data = gs.get_allocator().template alloc<__T[]>(size, std::forward<Args>(args)...);
        _size = size;
    }


    __T& operator[](size_type i)                { return _data[i]; }
    const __T& operator[](size_type i) const    { return _data[i]; }


    __T *data()             { return _data.get(); }
    const __T *data() const { return _data.get(); }

    __T &front()                { return _data[0]; }
    const __T &front() const    { return _data[0]; }

    __T &back()             { return _data[_size-1]; }
    const __T &back() const { return _data[_size-1]; }


    bool        empty() noexcept    { return _size==0; }
    size_type   size()  noexcept    { return _size;    }

    iterator begin() noexcept   { return iterator(data()); }
    iterator end()   noexcept   { return iterator(data() + _size); }

    reverse_iterator rbegin() noexcept      { return reverse_iterator(end()); }
    reverse_iterator rend()   noexcept      { return reverse_iterator(begin()); }

    const_iterator begin() const noexcept   { return const_iterator(data()); }
    const_iterator end()   const noexcept   { return const_iterator(data() + _size); }

    const_iterator cbegin() const noexcept  { return const_iterator(data()); }
    const_iterator cend()   const noexcept  { return const_iterator(data() + _size); }

    const_reverse_iterator rbegin() const noexcept  { return const_reverse_iterator(end()); }
    const_reverse_iterator rend()   const noexcept  { return const_reverse_iterator(begin()); }

    const_reverse_iterator crbegin() const noexcept { return const_reverse_iterator(end()); }
    const_reverse_iterator crend()   const noexcept { return const_reverse_iterator(begin()); }


    void fill(const value_type &v) {
        std::fill_n(begin(), size(), v);
    }



private:
    static auto fff() {     //for MSC
        gstack<>    gs;
        gs.alloc<__T[]>(1);
        return gs.alloc<__T[]>(1);
    }
    using data_ptr = decltype(fff());

protected:
    data_ptr    _data;



    unsigned    _size;
};




template<typename __T=char, unsigned __GSI=1, typename __Traits=std::char_traits<__T>>
using basic_gs_string = std::basic_string<__T, __Traits, gstack_allocator<__T,__GSI>>;

template<unsigned __GSI=1>  using gs_string    = basic_gs_string<char>;
template<unsigned __GSI=1>  using gs_wstring   = basic_gs_string<wchar_t>;
template<unsigned __GSI=1>  using gs_u16string = basic_gs_string<char16_t>;
template<unsigned __GSI=1>  using gs_u32string = basic_gs_string<char32_t>;



/*---------------------------------------------------------------------
    back_inserter
*/

template<typename _Container>
class back_insert_iterator;


template<typename __T>
class back_insert_iterator<__T*>
    : public std::iterator<std::output_iterator_tag, void, void, void, void>
{
protected:
    __T *&container;

public:
    explicit
    back_insert_iterator(__T *&__x) : container(__x) { }

    back_insert_iterator&
    operator=(const __T &__value)
    {
        *this->container++ = __value;
        return *this;
    }

    back_insert_iterator&
    operator=(__T &&__value)
    {
        *this->container++ = std::move(__value);
        return *this;
    }

    back_insert_iterator&
    operator*() { return *this; }

    back_insert_iterator&
    operator++() { return *this; }

    back_insert_iterator
    operator++(int) { return *this; }
};


template<typename _Container>
inline back_insert_iterator<_Container>
back_inserter(_Container& __x)
{ return back_insert_iterator<_Container>(__x); }






template<typename _Container>
class ptr_back_insert_iterator;


template<typename __T>
class ptr_back_insert_iterator<__T**>
    : public std::iterator<std::output_iterator_tag, void, void, void, void>
{
protected:
    __T **&container;

public:
    explicit
    ptr_back_insert_iterator(__T **&__x) : container(__x) { }

    ptr_back_insert_iterator&
    operator=(const __T &__value)
    {
        *this->container++ = &__value;
        return *this;
    }


    ptr_back_insert_iterator&
    operator*() { return *this; }

    ptr_back_insert_iterator&
    operator++() { return *this; }

    ptr_back_insert_iterator
    operator++(int) { return *this; }
};


template<typename _Container>
inline ptr_back_insert_iterator<_Container>
pointer_back_inserter(_Container& __x)
{ return ptr_back_insert_iterator<_Container>(__x); }





/*-------------------------------------------------------
    topological_sort
*/
template<typename List>
class topological_sort
{
    template <typename T,size_t,typename>
    struct map_sort { using map = typename topological_sort<T>::type_duped; };

    template<typename T, size_t, typename>
    struct map_subs {
        using map = std::tuple<type_map<type_map<List, type_filters::is_not_same, T>, type_filters::is_sub_of, T>>;
    };

    using topbases = type_map<List, type_filters::is_topbase_of, List>;
    using type_duped = tuple_cat_t<topbases, type_map<type_map<topbases, map_subs>, map_sort>>;

    template<typename L>
    friend class topological_sort;
public:
    using type = tuple_uniq_t<type_duped>;
    static constexpr bool isMI = !std::is_same< type, type_duped>::value;
};


/*---------------------------------------------------------------------
    class_tree
*/
namespace class_tree {///////////////////////////////

struct TypeInfoEntry {
    const void *(*cast)(const void*);
//  const std::type_info *type_info;
};
template<typename To,typename From>
const void *__dynamic_cast(const void *p) {
    return dynamic_cast<const To*>(static_cast<const From*>(p));
}

template<typename TypeList, typename Base>
struct typeInfo_list;

template<typename Base, typename...T>
class typeInfo_list<std::tuple<T...>,Base> {
    static const std::array<const TypeInfoEntry, std::tuple_size<std::tuple<T...>>::value> __list;
public:
    static const std::array<const TypeInfoEntry, std::tuple_size<std::tuple<T...>>::value> &get() {
        return __list;
    }
};

template<typename Base, typename...T>
const std::array<const TypeInfoEntry, std::tuple_size<std::tuple<T...>>::value> typeInfo_list<std::tuple<T...>,Base>::__list 
  = { TypeInfoEntry{
        &__dynamic_cast<T,Base>
        //&typeid(T)
      }...
  };
}// namespace class_tree/////////////////////////////


/*---------------------------------------------------------------------
    mf_init()
*/
namespace vane_detail {///////////////////////////////////////////////////////////////////////

    template<typename __VID=int>
    struct __MFCache {
        __MFCache() = delete;

        static void   set_vid(int *v)   { __vid = v; }
        static __VID &get_vid(int i)    {
            assert( __vid );
            return __vid[i];
        }

    private:
        static __VID *__vid;
    };

    template<typename __VID>
    __VID *__MFCache<__VID>::__vid;




    class _MF_init
    {
        template<typename=void>
        struct _init_flag {
            static bool __initialized;
        };

        static auto &get_initors() {
            static std::vector<initor_type>  __initors;
            return __initors;
        }

    public:
        using initor_type = std::function<void()>;

        static void init()
        {
            _init_flag<>::__initialized = true;

            for(auto f : get_initors() )
                f();
        }

        static void add_initor(initor_type f) { get_initors().push_back(f); }

        static
        void __check_mf_init() {
            if( !_init_flag<>::__initialized ) {
                fputs("\n\nERROR: vane::mf_init() is not called.\n\tterminating the program...\n\n", stderr);
                exit(1);
            }
        }
    };

    template<typename T>
    bool _MF_init::_init_flag<T>::__initialized;

    inline void __check_mf_init() {
        _MF_init::__check_mf_init();
    }

}//end-namespace vane_detail //////////////////////////////////////////////////////////////////



inline void mf_init() {
    vane_detail::_MF_init::init();
}




template<typename T, typename F>
constexpr
ptrdiff_t cast_diff() {
    return (ptrdiff_t)static_cast<T>((F)1) - 1;
}

template<typename T, typename F, typename E>
constexpr
ptrdiff_t cast_diff() {
    return (ptrdiff_t)static_cast<T>(static_cast<F>((E)1)) - 1;
}



/*---------------------------------------------------------------------
    vtype
*/

struct vtype
{
    using vtypeid_type  = int;

    vtype(vtypeid_type id) : _vtypeid(id) { }

    virtual ~vtype() = 0;

    vtypeid_type vtypeid() const { return _vtypeid; }

protected:
    vtypeid_type    _vtypeid;
};

inline vtype::~vtype() {}


template<typename T>
struct is_vtype : std::is_base_of<vtype,T> {};

template<typename T>
bool is_vtype_v = is_vtype<T>::value;




template<typename T, typename S>
T *get(S *v);
template<typename T, typename S>
T &get(S &v);




/*---------------------------------------------------------------------
    drtti -- domained RTTI
*/

template<typename Domains>
struct _Extents_domains {
    static const std::array<int,std::tuple_size<Domains>::value>  __extents;
};
    template<typename Domains>
    const std::array<int,std::tuple_size<Domains>::value>  _Extents_domains<Domains>::__extents = make_array<tuples_sizes<Domains>>();

template<typename Domains>
const auto &domains_sizes() {
    return _Extents_domains<Domains>::__extents;
}




#pragma pack(push,1)
template<typename ET=bool>
class drtti_table : __varray2d<ET>
{
    drtti_table() = delete;
    drtti_table(const drtti_table&) = delete;
    drtti_table(drtti_table&&) = delete;
    drtti_table &operator =(const drtti_table&) = delete;
    drtti_table &operator =(drtti_table&&) = delete;
public:

    constexpr drtti_table(int size) { this->_size = size; }

    using __varray2d<ET>::operator[];
    using __varray2d<ET>::size;
    using __varray2d<ET>::data;

    ET operator()(int i, int j) const {
        return (*this)[i][j];
    }
};
#pragma pack(pop)




template<typename Iter, typename Comp=std::less<typename std::iterator_traits<Iter>::value_type>>
Iter max_elements(Iter first, Iter last, Comp comp = Comp())
{
    if( first >= last ) return first;

    Iter head = first;
    Iter tail = first;
    for(; ++first != last ;) {
        if( comp(*head, *first) )
            std::swap( *(tail=head), *first);
        else if( !comp(*first, *head) )
            std::swap(*++tail, *first);
    }
    return tail + 1;
}


template<typename Col, typename Comp=std::less< decltype(std::declval<Col>()[0])>>
auto max_elements(Col &col, Comp comp = Comp()) {
    return max_elements(std::begin(col), std::end(col), comp);
}


template<typename Iter, typename Col, typename Comp=std::less<decltype(*std::declval<Iter>())>>
Iter max_elements_indexing(Iter first, Iter last, const Col &value, Comp comp=Comp()) {
    return max_elements(first, last, [&value,comp](auto x, auto y){ return comp(value[x], value[y]); });
}

template<typename Iter, typename Indices, typename Comp=std::less<decltype(*std::declval<Iter>())>>
auto max_elements_indexed(Iter first, Iter last, Indices indices, Comp comp=Comp() )
{
    auto cnt = last - first;
    std::iota(indices, indices + cnt, 0);

    return max_elements(indices, indices+cnt, [first,comp](auto i, auto j) { 
        return comp(first[i], first[j]);
    });
}

template<typename Col, typename Indices, typename Comp=std::less<decltype(std::declval<Col>()[0])>>
auto max_elements_indexed(Col col, Indices indices, Comp comp=Comp())
{
    return max_elements_indexed(std::begin(col), std::end(col), indices, comp);
}



template<typename Iter1, typename Iter2, typename BinOp>
int sum_mapxy(Iter1 from, Iter1 to, Iter2 against, BinOp op) {
    int sum = 0;
    for(; from != to ; ++from, ++against)
        sum += op(*from, *against);
    return sum;
}

template<typename Col, typename Col2, typename BinOp>
int sum_mapxy(const Col &x, const Col2 &y, BinOp op) {
    return sum_mapxy(std::begin(x), std::end(x), std::begin(y), op);
}

template<typename Col, typename T2, typename BinOp>
int sum_mapxy(const Col &x, const T2 *y, BinOp op) {
    return sum_mapxy(std::begin(x), std::end(x), y, op);
}




////////////////////////////////////////////////////////////////////////////////////
template<typename T>
struct array_size;

template<typename T, size_t N>
struct array_size<std::array<T,N>> {
    enum { value = N };
};

template<typename T, size_t N>
struct array_size<T[N]> {
    enum { value = N };
};

template<typename T, size_t N>
constexpr size_t ARRAY_SIZE(const T (&a)[N])                { return N; }

template<typename T, size_t N>
constexpr size_t ARRAY_SIZE(const std::array<T,N> &a)       { return N; }

template<typename T, size_t N>
constexpr size_t collection_size(const T (&a)[N])           { return N; }

template<typename T, size_t N>
constexpr size_t collection_size(const std::array<T,N> &a)  { return N; }

template<typename T>
constexpr size_t collection_size(const T &a)                { return a.size(); }



namespace drtti {/////////////////////////////////////////////////////////////////

template<typename TypeList>
struct _Make_typeid_list;

template<typename...Ts>
struct _Make_typeid_list<std::tuple<Ts...>>
{
    static const std::array<const std::type_info*, sizeof...(Ts)>  __data;
    static const std::array<const std::type_info*, sizeof...(Ts)>  &get() { return __data; }
};
    template<typename...Ts>
    const std::array<const std::type_info*, sizeof...(Ts)>
          _Make_typeid_list<std::tuple<Ts...>>::__data= {&typeid(Ts)...};

template<typename TypeList>
const auto &typeid_list() {
    return _Make_typeid_list<TypeList>::get();
}



template<typename Base, typename Iter, typename Iter2>
auto typeid_list(Iter first, Iter last, Iter2 list)
{
    auto head = list;
    for(; first < last ;++first, ++list) {
        assert( *first );
        *list = &typeid(*static_cast<const Base*>(*first));
    }

    return head;
}

template<typename Base, typename Col, typename Col2>
auto typeid_list(const Col &argv, Col2 &list) {
    return typeid_list<Base>(std::begin(argv), std::end(argv), std::begin(list));
}



template<typename TypeList, typename BaseType, typename=std::conditional_t<is_vtype<BaseType>::value, vtype, void> >
struct __Make_typeid_list;

template<typename...Ts, typename BaseType>
struct __Make_typeid_list<std::tuple<Ts...>,BaseType,void>
{
    static const std::array<const std::type_info*, sizeof...(Ts)>  __data;
    static const std::array<const std::type_info*, sizeof...(Ts)>  &get() { return __data; }
};
    template<typename...Ts, typename BaseType>
    const std::array<const std::type_info*, sizeof...(Ts)>
          __Make_typeid_list<std::tuple<Ts...>,BaseType,void>::__data= {&typeid(Ts)...};
template<typename...Ts, typename BaseType>
struct __Make_typeid_list<std::tuple<Ts...>,BaseType,vtype>
{
    static const std::array<const std::type_info*, sizeof...(Ts)>  __data;
    static const std::array<const std::type_info*, sizeof...(Ts)>  &get() { return __data; }
};

template<typename...Ts, typename BaseType>
const std::array<const std::type_info*, sizeof...(Ts)>
      __Make_typeid_list<std::tuple<Ts...>,BaseType,vtype>::__data= {&typeid(typename BaseType::template of<Ts>)...};


template<typename TypeList, typename BaseType>
const auto &_typeid_list() {
    return __Make_typeid_list<TypeList,BaseType>::get();
}

template<typename Domains, typename BaseTypes>
struct _Make_drtti_domains;

template<typename...Ds, typename...Bs>
struct _Make_drtti_domains<std::tuple<Ds...>, std::tuple<Bs...>>
{
    static const std::array<const std::type_info*const*, sizeof...(Ds)>  __data;
    static const std::array<const std::type_info*const*, sizeof...(Ds)>  &get() { return __data; }
};
    template<typename...Ds,typename...Bs>
    const std::array<const std::type_info*const*, sizeof...(Ds)>
            _Make_drtti_domains<std::tuple<Ds...>,std::tuple<Bs...>>::__data= {  &_typeid_list<Ds,remove_pointer_or_reference_t<Bs>>()[0]...};


template<typename Domains, typename BaseTypes>
const auto &__drtti_domains() {
    return _Make_drtti_domains<Domains,BaseTypes>::get();
}


template<typename TypeList, typename Base>
struct _Make_domain_casters;

template<typename Base, typename...Ts>
class _Make_domain_casters<std::tuple<Ts...>, Base>
{
    template<typename T>
    static void *dcast(Base *arg ){
        return dynamic_cast<T*>(arg);
    }
public:
    static const std::array<void*(*)(Base*), sizeof...(Ts)>  __data;
    static const std::array<void*(*)(Base*), sizeof...(Ts)>  &get() { return __data; }
};
    template<typename Base, typename...Ts>
    const std::array<void*(*)(Base*), sizeof...(Ts)>
          _Make_domain_casters<std::tuple<Ts...>,Base>::__data= { dcast<Ts>...};

template<typename TypeList, typename Base>
auto &domain_casters() {
    return _Make_domain_casters<TypeList,Base>::get();
}


////////////////////////////////////////////////////////////////////////////////////////////////
template<typename T=const drtti_table<>>
struct argv_ismap
{
    argv_ismap() = delete;
    argv_ismap(const argv_ismap&) = delete;
    argv_ismap(const argv_ismap&&) = delete;
    argv_ismap &operator =(const argv_ismap&) = delete;
    argv_ismap &operator =(argv_ismap&&) = delete;



    using element_type = T;
    int size() const { return _size; }

    int _size;
    T *_is_assignable_at[0];

    T &is_assignable_at(int i) const {
        assert( 0 <= i );
        assert( i < size() );
        return *_is_assignable_at[i];
    }

    template<typename Dst, typename Src>
    bool _is_assignable(const Dst &dst, const Src &src) const {
        for(int i=0; i<size(); ++i) {
            assert( 0 <= dst[i] );  assert( dst[i] < _is_assignable_at[i]->size() );
            assert( 0 <= src[i] );  assert( src[i] < _is_assignable_at[i]->size() );
            if( ! (*_is_assignable_at[i])[dst[i]][src[i]] )
                return false;
        }
        return true;
    }
    template<typename ET1, size_t SIZE1, typename ET2, size_t SIZE2>
    bool is_assignable(const std::array<ET1,SIZE1> &dst, const std::array<ET2,SIZE2> &src) const {
        assert( SIZE1 == size() );
        assert( SIZE2 == size() );
        return _is_assignable(dst, src);
    }
    template<typename ET1, size_t SIZE1, typename ET2, size_t SIZE2>
    bool is_assignable(const std::array<ET1,SIZE1> *dst, const std::array<ET2,SIZE2> *src) const {
        return is_assignable(*dst, *src);
    }

    template<typename ET1, size_t SIZE1, typename ET2, size_t SIZE2>
    bool is_assignable(const ET1 (&dst)[SIZE1], const ET2 (&src)[SIZE2]) const {
        assert( SIZE1 == size() );
        assert( SIZE2 == size() );
        return _is_assignable(dst, src);
    }

    template<typename ET1, typename ET2>
    bool is_assignable(const ET1 *dst, const ET2 *src) const {
        return _is_assignable(dst, src);
    }

    template<typename Dst, typename Src>
    bool operator()(const Dst &dst, const Src &src) const {
        return is_assignable(dst, src);
    }
};

struct map_is_base_of {
    template<typename B, typename D>
    struct mapper {
        static constexpr bool map() { return std::is_base_of<B,D>::value; }
    };
};

}//end-namespace drtti ////////////////////////////////////////////////////////////



////////////////////////////////////////////////////////////////////////////////////////////////
#pragma pack(push,1)
template<typename Map, typename Set, typename ET=bool>
class __drtti_ismap
{
            template<typename _Set>
            struct __Build_data;

            template<typename...Ti>
            struct __Build_data<std::tuple<Ti...>>
            {
                static constexpr int size() { return sizeof...(Ti); }

                template<typename T>
                struct __Build_row {
                    static constexpr
                    std::array<ET,size()> get() {
                        return { Map::template mapper<T,Ti>::map()... };
                    }
                };

                static constexpr
                multi_array<ET,size(),size()> get() {
                    return { __Build_row<Ti>::get()... };
                }
            };

    __drtti_ismap() = delete;
public:
    static constexpr int size() { return std::tuple_size<Set>::value; }

    drtti_table<ET>                 _instance;
    multi_array<ET, size(),size()>  _data;

    static const __drtti_ismap      __storage;
    static const drtti_table<ET> &get_instance() { return __storage._instance; }
    static constexpr multi_array<ET,size(),size()> get_data() { return __Build_data<Set>::get();};
};
#pragma pack(pop)

template<typename Map, typename Set, typename ET>
const
__drtti_ismap<Map, Set, ET>
__drtti_ismap<Map, Set, ET>::__storage = {
    { size() },
    __Build_data<Set>::get()
};


template<typename Map, typename Set, typename ET=bool>
const auto &drtti_ismap() {
    return __drtti_ismap<Map, Set, ET>::get_instance();
}



//////////////////////////////////////////////////////////////////////////////////////////////
#pragma pack(push,1)
template<typename ET, template<typename>class _drtti_table, typename...Domains, typename Mapper>
struct make_static<drtti::argv_ismap<const _drtti_table<ET>>, std::tuple<Domains...>, Mapper> {

    static const drtti::argv_ismap<_drtti_table<ET>>    &get_instance() { return __storage._instance; }
    static constexpr int size() { return sizeof...(Domains); }

    drtti::argv_ismap<_drtti_table<ET>> _instance;
    std::array<const _drtti_table<ET>*, size()> _data;

    static const make_static    __storage;

};
#pragma pack(pop)

template<typename ET, template<typename>class _drtti_table, typename...Domains, typename Mapper> const
make_static<drtti::argv_ismap<const _drtti_table<ET>>, std::tuple<Domains...>, Mapper>
make_static<drtti::argv_ismap<const _drtti_table<ET>>, std::tuple<Domains...>, Mapper>::__storage = {
    { size(), {}},
    { &drtti_ismap<Mapper, Domains>()... }
};



template<typename Iter, typename IsAssignable>
Iter reduce_sigs_short(Iter first, Iter last, const IsAssignable &is_assignable)
{
    --last;
    while( first < last ) {
        for(Iter j=first+1;;) {
            if( is_assignable(*j, *first) )
                *j = *last--;
            else if( is_assignable(*first, *j) ) {
                *first = *last--;
                break;
            }
            else
                ++j;

            if( j > last ) {
                ++first;
                break;
            }
        }
    }

    return last+1;
}

template<typename Col, typename IsAssignable>
auto reduce_sigs_short(Col &sigs, const IsAssignable &is_assignable) {
return reduce_sigs_short(std::begin(sigs), std::end(sigs), is_assignable);
}



template<typename Iter, typename Iter2, typename IsAssignable>
void reduce_sigs_exhaustive(Iter first, Iter last, Iter2 bag, const IsAssignable &is_assignable) {
    std::copy_if(first, last, bag, [first,last,&is_assignable](auto x){
        return ! std::any_of(first,last, [x,&is_assignable](auto y){
            return x!=y && is_assignable(x, y);
        });
    });
}
template<typename Col, typename Iter, typename IsAssignable>
void reduce_sigs_exhaustive(const Col &sigs, Iter bag, const IsAssignable &is_assignable) {
    reduce_sigs_exhaustive(std::begin(sigs), std::end(sigs), bag, is_assignable);
}


template<typename Domains, typename ArgTypes, typename Iter, typename Castmaps>
unsigned reduce_sigs(Iter sig_first, Iter sig_last, const Castmaps &castmaps, int castmap_cell_invalid, const std::type_info *arg_tidv[] )
{
    using sig_type = std::remove_const_t<std::remove_reference_t<decltype(*sig_first)>>;
    constexpr auto ARGC = std::tuple_size<Domains>::value;

    gstack<>  gs;


    auto &argv_is_assignable = make_static<drtti::argv_ismap<>, Domains, drtti::map_is_base_of>::get_instance();
    unsigned sigc = sig_last - sig_first;
    auto  sigs = gs.alloc<const sig_type*[]>(sigc);
    const sig_type **last = &sigs[0];

    reduce_sigs_exhaustive(sig_first, sig_last, vane::pointer_back_inserter(last), argv_is_assignable);

    sigc = last - &sigs[0];
    if( 1 < sigc )
    {
        using CELL = int;
        auto buffer       = gs.alloc<CELL[]>(2*sigc);
        auto sig_matches  = buffer.get();
        auto indices      = buffer.get() + sigc;

        const auto &rt_domains = drtti::__drtti_domains<Domains,ArgTypes>();
        assert( arg_tidv );

        for(int i=0; i<sigc ;++i) {
            std::array<const std::type_info*,ARGC>  sig_tidv;
            const auto &sig = *sigs[i];
            for(int j=0; j<ARGC ;++j)
                sig_tidv[j] = rt_domains[j][sig[j]];

            sig_matches[i] = sum_mapxy(&sig_tidv[0], &sig_tidv[ARGC], arg_tidv, std::equal_to<const std::type_info*>());
        }


        auto gathered = gs.alloc<const sig_type*[]>(sigc);
        auto do_reduce = [&sigc,sigs=sigs.get(),sig_matches,indices,gathered=gathered.get()]() {
            sigc = max_elements_indexed(sig_matches, sig_matches+sigc, indices) - indices;

            for(int i=0; i<sigc ;++i) gathered[i] = sigs[indices[i]];
            for(int i=0; i<sigc ;++i) sigs[i]     = gathered[i];
        };

        do_reduce();

        if( 1 < sigc ) 
        {
            for(int i=0; i<sigc ;++i) {
                sig_matches[i] = sum_mapxy(*sigs[i], &castmaps[0], [castmap_cell_invalid](int x, auto cmap){
                    return cmap[x] != castmap_cell_invalid;
                });
            }

            do_reduce();
        }
    }



    auto newsigs = gs.alloc<sig_type[]>(sigc);
    for(int i=0; i<sigc ;++i)  newsigs[i]    = *sigs[i];
    for(int i=0; i<sigc ;++i) *(sig_first+i) = newsigs[i];

    return sigc;
}

template<typename Domains, typename ArgTypes, typename Collection, typename Castmaps>
auto reduce_sigs(Collection &sigcol, const Castmaps &castmaps, int castmap_cell_invalid, const std::type_info *arg_tidv[] )
{
    return reduce_sigs<Domains,ArgTypes>(std::begin(sigcol), std::end(sigcol), castmaps, castmap_cell_invalid, arg_tidv );
}




namespace drtti {////////////////////////////////////////////////////////////

template<typename __Domain, typename Iter>
unsigned find_closest_bases(Iter first, Iter last)
{
    return reduce_sigs_short(first, last, drtti_ismap<drtti::map_is_base_of, __Domain>()) - first;
}


namespace vane_detail {
    template<typename Iter>
    int *__gather_base_indices(Iter castmap, int n, int *bases, decltype(castmap[0]) _CASTMAP_CELL_INVALID) {
        for(int i=0; i < n  ;++i)
            if( *(castmap+i) != _CASTMAP_CELL_INVALID )
                *bases++ = i;
        return bases;
    }
}

template<typename __Domain, typename __CastmapRow>
unsigned find_closest_bases_from_castmap(const __CastmapRow &castmap, int *bases, decltype(castmap[0]) _CASTMAP_CELL_INVALID) {

    static_assert( std::tuple_size<__CastmapRow>::value == std::tuple_size<__Domain>::value, "");

    int *last = vane_detail::__gather_base_indices(std::begin(castmap), std::tuple_size<__Domain>::value, bases, _CASTMAP_CELL_INVALID);
    return find_closest_bases<__Domain>(bases, last);
}

}//end-namespace drtti ////////////////////////////////////////////////////////////






/*---------------------------------------------------------------------
    virtual_<>
*/
struct __virt : vtype
{
    virtual ~__virt() = 0;

    __virt(vtypeid_type id) : vtype(id) {}
};

inline __virt::~__virt() {}




namespace vane_detail {
    template<typename __Domain, typename __BaseType, bool __MI>
    struct __VTypemap_typemap_base;
}







template<typename __BasePoly>
struct virtual_;



template<typename __BasePoly>
struct virtual_ : __virt
{
protected:
    using _Self = virtual_<__BasePoly>;
    using _Super = __virt;


    virtual_(vtypeid_type id) : __virt(id) {}

public:
    using base_type      = _Self;
    using base_type_poly = __BasePoly;


    template<typename __T, typename __Root=virtual_<__BasePoly>>
    struct of : __Root, __T
    {
        static_assert(std::is_base_of<__BasePoly,__T>::value, "vane::virtual_<Base>::of<T>: invalid argument type T   //base type mismatch");

    protected:
        using _Self  = of<__T,__Root>;
        using _Super = virtual_<__BasePoly>;

    public:
        using data_type = __T;

        of() : _Super(__vtypeid)
        { 
            _init();
        }

        template<typename...Args>
        of(Args&&...args) : _Super(__vtypeid), data_type{std::forward<Args>(args)...}
        { 
            _init();
        }


        static vtypeid_type static_vtypeid() { return __vtypeid; }

    private:
        void _init() {
            this->_vtypeid = __vtypeid;
            this->_vdiff = (char*)static_cast<base_type_poly*>(this) - (char*)this;

            assert( ((char*)static_cast<base_type_poly*>(this) - (char*)this) <=  std::numeric_limits<vdiff_type>::max() );
            assert( ((char*)static_cast<base_type_poly*>(this) - (char*)this) >=  std::numeric_limits<vdiff_type>::min() );
        }


        static vtypeid_type  __vtypeid;
    };


    operator __BasePoly&() {
        return *get<__BasePoly>(this);
    }

    operator const __BasePoly&() const {
        return *get<const __BasePoly>(this);
    }


    using vdiff_type = int;
    vdiff_type  _vdiff;


protected:
    static int __vtypeid_last;

    template<typename __Domain, typename __BaseType, bool __MI>
    friend struct vane::vane_detail::__VTypemap_typemap_base;
};


template<typename __BasePoly>
int virtual_<__BasePoly>::__vtypeid_last = 0;


template<typename __BasePoly>
template<typename __T, typename __Root>
typename __virt::vtypeid_type  virtual_<__BasePoly>::of<__T,__Root>::__vtypeid = ++__vtypeid_last;





////////////////////////////////////////////////////////////////////////////////////////////////////////
template<typename X, typename B>
constexpr inline
X *get(virtual_<B> *v) {
    assert( (X*)((char*)v + cast_diff<X*, typename virtual_<B>::template of<X>*>()) == static_cast<typename virtual_<B>::template of<X>*>(v) );
    return (X*)((char*)v + cast_diff<X*, typename virtual_<B>::template of<X>*>());
}

template<typename X, typename B>
inline
X &get(virtual_<B> &v) {
    return *get<X*>(&v);
}

template<typename X, typename B>
inline
const X *get(const virtual_<B> *v) {
    assert( (const X*)((char*)v + cast_diff<const X*, const typename virtual_<B>::template of<X>*>()) == static_cast<const typename virtual_<B>::template of<X>*>(v) );
    return (const X*)((char*)v + cast_diff<const X*, const typename virtual_<B>::template of<X>*>());
}

template<typename X, typename B>
inline
const X &get(const virtual_<B> &v) {
    return *get<X>(&v);
}




template<typename T>
struct is_virt : std::is_base_of<__virt,T> {};





/*---------------------------------------------------------------------
    var<>
*/

struct __var : vtype
{
    __var(vtypeid_type id) : vtype(id) {}
protected:
    virtual ~__var() = 0;
    using vtype::vtype;

    // data_container
    template<typename T>
    struct data_container : std::conditional_t<std::is_class<T>::value, T,std::tuple<T>> {
        using Data = std::conditional_t<std::is_class<T>::value, T,std::tuple<T>>;
        template<typename...Args> constexpr
        data_container(Args&&...args) : Data{std::forward<Args>(args)...} {}

        operator T&() {
            return std::get<0>(*this);
        }
        operator const T&() const { 
            return std::get<0>(*this);
        }
    };
};

inline __var::~__var() {}






///////////////////////////////////////////////////////////////////

template<typename...__Tags>
struct var : __var
{
protected:

    using _Self = var<__Tags...>;
    using _Var = _Self;

    using __var::__var;

protected:
    var(unsigned tid) : __var(tid) { assert(tid); }


public:

    template<typename T, typename __Root=_Self>
    struct of : __Root, data_container<T>
    {
    private:
        using Super = __Root;
        using _data_container = data_container<T>;

    protected:
        using typename _data_container::Data;

    public:
        using data_type = T;
        using data_container_type = data_container<T>;

    public:
        of() : Super(__vtypeid)  {}

        template<typename...Args>
        of(Args&&...args) : Super(__vtypeid), _data_container(std::forward<Args>(args)...)
        { }


    private:
        using Super::_vtypeid;
        static vtypeid_type  __vtypeid;
    };

protected:
    static int __vtypeid_last;

    template<typename __Domain, typename __BaseType, bool __MI>
    friend struct vane::vane_detail::__VTypemap_typemap_base;
};



template<typename... __Tags>
int  var<__Tags...>::__vtypeid_last;


template<typename... __Tags>
template<typename T, typename __Root>
__var::vtypeid_type var<__Tags...>::of<T,__Root>::__vtypeid = ++__vtypeid_last;




namespace _vane_detail {///////////////////////////////////////////////////////////////

    template<typename X, typename Var, bool=std::is_base_of<Var,X>::value>
    struct __var_getter;

    template<typename X, typename...Tags>
    struct __var_getter<X, var<Tags...>,false>
    {
        static X &get(var<Tags...> &v) {
            return *static_cast<typename var<Tags...>::template of<X>*>(&v);
        }

        static X *get(var<Tags...> *v) {
            return &get(*v);
        }
    };

    template<typename X, typename...__Tags>
    struct __var_getter<X, var<__Tags...>,true>
    {
        static X &get(var<__Tags...> &v) {
            return static_cast<X&>(v);
        }

        static X *get(var<__Tags...> *v) {
            return &get(*v);
        }
    };
    template<typename X, typename...Tags>
    struct __var_getter<X, const var<Tags...>,false>
    {
        static const X &get(const var<Tags...> &v) {
            return *static_cast<const typename var<Tags...>::template of<const X>*>(&v);
        }

        static const X *get(const var<Tags...> *v) {
            return &get(*v);
        }
    };

    template<typename X, typename...__Tags>
    struct __var_getter<X, const var<__Tags...>,true>
    {
        static const X &get(const var<__Tags...> &v) {
            return static_cast<X&>(v);
        }

        static const X *get(const var<__Tags...> *v) {
            return &get(*v);
        }
    };

}//end-namespace _vane_detail {//////////////////////////////////////////////


template<typename X, typename...Tags>
inline
X &get(var<Tags...> &v) {
    return _vane_detail::__var_getter<X,var<Tags...>>::get(v);
}

template<typename X, typename...Tags>
inline
X *get(var<Tags...> *v) {
    return _vane_detail::__var_getter<X,var<Tags...>>::get(v);
}

template<typename X, typename...Tags>
inline
const X &get(const var<Tags...> &v) {
    return _vane_detail::__var_getter<const X,const var<Tags...>>::get(v);
}

template<typename X, typename...Tags>
inline
const X *get(const var<Tags...> *v) {
    return _vane_detail::__var_getter<const X,const var<Tags...>>::get(v);
}

template<typename X, typename T>
inline
X &get(T &v) {
    return static_cast<X&>(v);
}

template<typename X, typename T>
inline
X *get(T *v) {
    return &get<X>(*v);
}


template<typename X, typename T>
inline
const X &get(const T &v) {
    return static_cast<const X&>(v);
}

template<typename X, typename T>
inline
const X *get(const T *v) {
    return &get<X>(*v);
}


template<typename T>
struct is_var : std::is_base_of<__var,T> {};



template <typename T, typename V, typename...Args>
inline std::shared_ptr<typename V::template of<T>>
make_shared(Args&&...args) {
    return std::make_shared<typename V::template of<T>>(std::forward<Args>(args)...);
}

template <typename T, typename V, typename...Args>
inline std::unique_ptr<typename V::template of<T>>
make_unique(Args&&...args) {
    return std::make_unique<typename V::template of<T>>(std::forward<Args>(args)...);
}



namespace vane_detail {///////////////////////////////////////////////////////////////////////////////////////////

/*---------------------------------------------------------------------
    FX utils
*/
namespace FX_utils {

    template<typename FX, typename RArgs, typename Args=remove_consts<RArgs>, class=std::__void_t<>>
    struct __FX_exists_callable : std::false_type { 
        using args = Args;
    };

    template< typename FX, typename RArgs, typename...As >
    struct __FX_exists_callable<FX, RArgs, std::tuple<As...>, std::__void_t<decltype( std::declval<FX>()(std::declval<As>()...) )> >
        : std::true_type {
        using args = std::tuple<As...>;
    };

    template<typename FX, typename RArgs, typename Args=remove_consts<RArgs>, class=std::__void_t<>>
    struct __FX_exists_error_NO_MATCH: std::false_type { 
        using args = Args;
    };

    template< typename FX, typename RArgs, typename...As >
    struct __FX_exists_error_NO_MATCH<FX, RArgs, std::tuple<As...>, std::__void_t<decltype( std::declval<FX>().error_NO_MATCH(std::declval<As>()...) )> >
        : std::true_type {
        using args = std::tuple<As...>;
    };

    template<typename FX, typename RArgs, typename Args=remove_consts<RArgs>, class=std::__void_t<>>
    struct __FX_exists_error_OOD: std::false_type { 
        using args = Args;
    };

    template< typename FX, typename RArgs, typename...As >
    struct __FX_exists_error_OOD<FX, RArgs, std::tuple<As...>, std::__void_t<decltype( std::declval<FX>().error_OOD(std::declval<As>()...) )> >
        : std::true_type {
        using args = std::tuple<As...>;
    };


    template<typename FX, typename Args, class=std::__void_t<>>
    struct FX_exists_poly_callable : std::false_type { };

    template< typename FX, typename...As >
    struct FX_exists_poly_callable<FX, std::tuple<As...>, std::__void_t<decltype( std::declval<FX>()(std::declval<As*>()...) )> >
        : std::true_type { };

    template<typename FX, typename Args, class=std::__void_t<>>
    struct FX_exists_sig : std::false_type { };

    template< typename FX, typename...As >
    struct FX_exists_sig<FX, std::tuple<As...>, std::__void_t<decltype( std::declval<typename resolve_signature<typename FX::type>::return_type(FX::*&)(As...)>() = &FX::operator() )> >
        : std::true_type { 
        using type = decltype( std::declval<typename resolve_signature<typename FX::type>::return_type(FX::*&)(As...)>() );
    };

}//end-namespace FX_utils ////////////////////////////////////////////////////////



/*---------------------------------------------------------------------
    runtime errors
*/
struct __void_return_t{};



template<typename __return_type, typename __MF>
struct FxCaller
{
    using Return_type = __return_type;


    template<typename Fx, typename...Args>
    static
    Return_type call(Fx *fx, Args&&...args) {
        return (*fx)(std::forward<Args>(args)...);
    }


    template<typename Fx, typename...Args>
    static
    Return_type
    error_OOD(Fx *fx, Args&&...args)    //out-of-domain
    {
        return fx->error_OOD(std::forward<Args>(args)...);
    }

    template<typename Fx, typename...Args>
    static
    Return_type
    error_NO_MATCH(Fx *fx, Args&&...args)   //no mathcing functions found
    {
        return fx->error_NO_MATCH(std::forward<Args>(args)...);
    }
};




template<typename __MF>
struct FxCaller<void,__MF>
{
    using Return_type = __void_return_t;


    template<typename Fx, typename...Args>
    static
    Return_type call(Fx *fx, Args&&...args) {
        (*fx)(std::forward<Args>(args)...);

        return Return_type{};
    }


    template<typename Fx, typename...Args>
    static
    Return_type
    error_OOD(Fx *fx, Args&&...args)    //out-of-domain
    {
        fx->error_OOD(std::forward<Args>(args)...);

        return Return_type{};
    }

    template<typename Fx, typename...Args>
    static
    Return_type
    error_NO_MATCH(Fx *fx, Args&&...args)   //no mathcing functions found
    {
        fx->error_NO_MATCH(std::forward<Args>(args)...);

        return Return_type{};
    }
};



template<typename __MF, bool=FX_utils::__FX_exists_error_OOD<typename __MF::FX, typename __MF::VSig::arg_types>::value>
struct _Error_OOD_Helper;

template<typename __MF>
struct _Error_OOD_Helper<__MF,false>
{
    template<typename...Args>
    static auto call(typename __MF::FX *fx, Args&&...args) {
        return __MF::__error_OOD_default(fx, std::forward<Args>(args)...);
    }
};

template<typename __MF>
struct _Error_OOD_Helper<__MF,true>
{
    template<typename...Args>
    static auto call(typename __MF::FX *fx, Args&&...args) {
        return FxCaller<typename __MF::VSig::return_type,__MF>::error_OOD(fx, std::forward<Args>(args)...);
    }
};


template<typename __MF, bool=FX_utils::__FX_exists_error_NO_MATCH<typename __MF::FX, typename __MF::VSig::arg_types>::value>
struct _Error_NO_MATCH_Helper;

template<typename __MF>
struct _Error_NO_MATCH_Helper<__MF,false>
{
    template<typename...Args>
    static auto call(typename __MF::FX *fx, Args&&...args) {
        return __MF::__error_NO_MATCH_default(fx, std::forward<Args>(args)...);
    }
};

template<typename __MF>
struct _Error_NO_MATCH_Helper<__MF,true>
{
    template<typename...Args>
    static auto call(typename __MF::FX *fx, Args&&...args) {
        return FxCaller<typename __MF::VSig::return_type,__MF>::error_NO_MATCH(fx, std::forward<Args>(args)...);
    }
};

}//end-namespace vane_detail /////////////////////////////////////////////////////////////////////////////////////////



//exceptions
struct multifunc_error : std::runtime_error {
    multifunc_error(const char *m="multi-func error") : runtime_error(m) {}
};

struct bad_call : multifunc_error {
    bad_call(const char *m="multi-func error: invalid call: no matching function or ambiguous call") 
        : multifunc_error(m) {}
};

struct bad_type : bad_call {
    bad_type(const char *m="multi-func error: invalid argument type: out of the type-domain")
        : bad_call(m) {}
};

struct multi_inheritance_detected : multifunc_error {
    multi_inheritance_detected (const char *m="multi-func error: multiple-inheritance detected")
        : multifunc_error(m) {}
};


namespace vane_detail {///////////////////////////////////////////////////////////////////////////////////////////

struct __vane_error {
    static __attribute__((noreturn)) __attribute__((noinline)) 
    void __throw_OOD() {
        throw bad_type();
    }

    static __attribute__((noreturn)) __attribute__((noinline)) 
    void __throw_NO_MATCH() {
        throw bad_call();
    }

    static __attribute__((noreturn)) __attribute__((noinline)) 
    void __throw_MI_detected() {
        throw multi_inheritance_detected();
    }
};



inline
void __throw_OOD() {
    __vane_error::__throw_OOD();
}

inline
void __throw_NO_MATCH() {
    __vane_error::__throw_NO_MATCH();
}

inline
void __throw_MI_detected() {
    __vane_error::__throw_MI_detected();
}

}//end-namespace vane_detail /////////////////////////////////////////////////////////////////////////////////////////



/*---------------------------------------------------------------------
    vtypemap<>'s
*/

template<typename __T>
void vector_validate_at(std::vector<__T> &v, size_t index, const __T &value) {
    if( v.size() <= index ) 
        v.resize(index+1, value);
}

namespace vane_detail {///////////////////////////////////////////////////////////////////////////////////////////

template<typename __TypeMap, bool __MI>
struct __VType_base_caster;


//___MI
template<typename __TypeMap>
struct __VType_base_caster<__TypeMap, true>
{
    template<typename __T, typename __BaseType> 
    static __T *cast(vtype::vtypeid_type vid, const __BaseType *__s)
    {
        static_assert(std::is_base_of<typename __TypeMap::base_type, __BaseType>::value, "");
        static_assert( 0 <= tuple_index<std::remove_const_t<__T>,typename __TypeMap::Domain>, "argument type out of the domain");

        __TypeMap::init();

        return  (__T*)((char*)__s + __TypeMap::get()._castmap[vid][tuple_index<std::remove_const_t<__T>,typename __TypeMap::Domain>]);
    }
};


//___SI
template<typename __TypeMap>
struct __VType_base_caster<__TypeMap, false>
{
    template<typename __T, typename __BaseType>
    static __T *cast(vtype::vtypeid_type vid, const __BaseType *__s) {
        static_assert(std::is_base_of<typename __TypeMap::base_type, __BaseType>::value, "");

        __TypeMap::init();

        return  vane::get<__T>(const_cast<std::remove_const_t<__BaseType>*>(__s));
    }
};




/////////////////////////////////////////////////////////////////////////////////////
template<typename __Cell>
struct _var_castmap_common
{
    using castmap_cell_type = __Cell;
    static_assert(std::is_signed<__Cell>::value, "");
    enum {
        _CASTMAP_CELL_UNDEFINED = (__Cell)~(((typename std::make_unsigned_t<__Cell>)-1)>>1), 
        _CASTMAP_CELL_INVALID   = (__Cell)~(((typename std::make_unsigned_t<__Cell>)-1)>>2),
    };

    template<typename Entry>
    static void __build_castmap_row(__Cell *row, int size, const Entry *entries, const void *arg, __Cell invalidValue=_CASTMAP_CELL_INVALID) {
        for(int i=0; i<size ;++i) {
            char *c = (char*)(*entries[i].cast)(arg);
            int diff = c-(char*)arg;
            row[i] = c ? diff : invalidValue;
        }
    }

    template<typename Row , typename TypeInfoList>
    static void __build_castmap_row(Row &row, const TypeInfoList &list, const void *arg, __Cell invalidValue=_CASTMAP_CELL_INVALID) {
        __build_castmap_row(&*std::begin(row), row.size(), &*std::begin(list), arg, invalidValue);
    }

    template<typename Domain, typename Row, typename BaseType>
    static void __build_castmap_row(Row &row, BaseType *arg) {
        __build_castmap_row(
            &*std::begin(row),
            row.size(),
            &*std::begin(class_tree::typeInfo_list<Domain, BaseType>::get()),
            arg
        );
    }
};


template<typename __Domain, typename __Arg, bool __isMI=false>
struct __Check_castmap {
    template<typename Table, typename EH>
    static void check(const Table &table, const __Arg *arg, bool &r, EH) {}
};

template<typename __Domain, typename __Arg>
class __Check_castmap<__Domain, __Arg, false>
{
    static void __throw_castmap_check_error() {
        __throw_MI_detected();
    }

    struct _Check1 {
        template<typename T, typename Cell>
        static
        void _check1(Cell cell, const __Arg *arg) {
            if( cell != _var_castmap_common<Cell>::_CASTMAP_CELL_INVALID ) {
                if( ((char*)get<T>(const_cast<__Arg*>(arg)) - (char*)arg) != cell )
                    __throw_castmap_check_error();
            }
        }
    };

    template<typename Table, size_t...Is>
    static void check(const Table &table, const __Arg *arg,  std::index_sequence<Is...>) {
        std::make_tuple((_Check1::template _check1<std::tuple_element_t<Is,__Domain>>(table[Is], arg),0)...);
    }

public:

    template<typename Table>
    static void check(const Table &table, const __Arg *arg) {
        check(table, arg, std::make_index_sequence<std::tuple_size<__Domain>::value>());
    }

};



//////////////////////////////////////////////////////////////////
template<typename __Domain, typename __BaseType, bool __MI>
struct __VTypemap_typemap_base
{
public:
    using typemap_cell_type = int;
    std::vector<typemap_cell_type>  _typemap;
    std::vector<typemap_cell_type>  _multimap;

public:
    enum {
        _TYPEMAP_CELL_UNDEFINED     = (typemap_cell_type)0,

        _MULTIMAP_CELL_UNDEFINED    = (typemap_cell_type)-1,
        _MULTIMAP_CELL_INVALID      = (typemap_cell_type)~(((typename std::make_unsigned_t<typemap_cell_type>)-1)>>1),
    };

public:
    static unsigned make_type_index(unsigned c) { return  c; }

    int _get_multi_index(const __BaseType *arg) {
        return this->_multimap[ arg->_vtypeid ];
    }

protected:
    void init_typemap(int vid) {
        vector_validate_at(_typemap,  vid, (typemap_cell_type)_TYPEMAP_CELL_UNDEFINED);
        vector_validate_at(_multimap, vid, (typemap_cell_type)_MULTIMAP_CELL_UNDEFINED);
    }

    __VTypemap_typemap_base() {
        _MF_init::add_initor([this]{ this->init_typemap(__BaseType::__vtypeid_last); });
    }
};



/////////////////////////////////////////////////////////////////////////////////////////////////////////
template<typename __Domain, typename __BaseType, bool __MI>
struct _VTypemap_vtype_typemap;



//MI
template<typename __Domain, typename __BaseType>
struct _VTypemap_vtype_typemap<__Domain, __BaseType, true>
    :  __VTypemap_typemap_base<__Domain, __BaseType, true>
{
    enum { __isMI = true };

    auto _get_type_index(const __BaseType *arg) {
        assert( arg );
        assert( arg->vtypeid() < this->_typemap.size() );
        return this->_typemap[arg->vtypeid()];
    }
    auto _get_type_index(const __BaseType &arg) {
        assert( arg.vtypeid() < this->_typemap.size() );
        return this->_typemap[arg.vtypeid()];
    }


    template<typename CastmapRow>
    int update_typemap(int vid, const __BaseType *arg, const CastmapRow &castmap)
    {
        static_assert( std::tuple_size<CastmapRow>::value == std::tuple_size<__Domain>::value, "");
        assert( vid == arg->vtypeid() );
        assert( vid < this->_typemap.size()  );
        assert( vid < this->_multimap.size() );

        int mix = this->_multimap[vid];
        if( mix != this->_MULTIMAP_CELL_UNDEFINED )
        {
            return mix;
        }

        int bases[std::tuple_size<__Domain>::value];

        int bases_cnt = drtti::find_closest_bases_from_castmap<__Domain>(castmap, bases, _var_castmap_common<std::remove_reference_t<decltype(castmap[0])>>::_CASTMAP_CELL_INVALID);    //TODO:...
        if( bases_cnt == 0 ) {
            this->_multimap[vid] = mix = this->_MULTIMAP_CELL_INVALID;
        }
        else if( bases_cnt == 1 ) {
            this-> _typemap[vid] = bases[0]+1;
            this->_multimap[vid] = mix = 0;
        }
        else {
            this-> _typemap[vid] = this->_TYPEMAP_CELL_UNDEFINED;
            this->_multimap[vid] = mix = vid;
        }

        return mix;
    }
};//end-struct



//SI
template<typename __Domain, typename __BaseType>
struct _VTypemap_vtype_typemap<__Domain, __BaseType, false>
    :  __VTypemap_typemap_base<__Domain, __BaseType, false>
{
    enum { __isMI = false };

    auto _get_type_index(const __BaseType *arg) {
        assert( arg );
        assert( arg->vtypeid() < this->_typemap.size() );
        return this->_typemap[arg->vtypeid()];
    }
    auto _get_type_index(const __BaseType &arg) {
        assert( arg.vtypeid() < this->_typemap.size() );
        return this->_typemap[arg.vtypeid()];
    }


    template<typename CastmapRow>
    int update_typemap(int vid, const __BaseType *arg, const CastmapRow &castmap)
    {
        static_assert( std::tuple_size<CastmapRow>::value == std::tuple_size<__Domain>::value, "");

        assert( vid == arg->vtypeid() );
        assert( vid < this->_typemap.size()  );
        assert( vid < this->_multimap.size() );

        int mix = this->_multimap[vid];
        if( mix != this->_MULTIMAP_CELL_UNDEFINED ) //-1
        {
            return mix;
        }

        

        static_assert( std::is_same<decltype(castmap[0]+0),int>::value, "");
        int bases[std::tuple_size<__Domain>::value];

        int bases_cnt = drtti::find_closest_bases_from_castmap<__Domain>(castmap, bases, _var_castmap_common<std::remove_reference_t<decltype(castmap[0])>>::_CASTMAP_CELL_INVALID);
        if( bases_cnt == 0 ) {
            this->_multimap[vid] = mix = this->_MULTIMAP_CELL_INVALID;
        }
        else if( bases_cnt == 1 ) {
            this-> _typemap[vid] = bases[0]+1;
            this->_multimap[vid] = mix = 0;
        }
        else {
            __throw_MI_detected();
        }

        return mix;
    }

};//end-struct







/////////////////////////////////////////////////////////////////////////////////////////////////////////
//___SI
template<typename __Domain, typename __BaseType, typename __CellType, bool __MI>
struct _VTypemap_base_castmap
    : _var_castmap_common<__CellType>
{
    using castmap_type = std::array<__Domain,0>;    //dummy
    using castmap_cell_type = __CellType;

    template<typename CastmapRow>
    static void check_castmap_row(const __BaseType *arg, CastmapRow &row) {
        __Check_castmap<__Domain,__BaseType>::check(row, arg);
    }
};




        template<typename __Castmap, typename InitFunc>
        void __update_castmap(__Castmap &_castmap, int vid, InitFunc init_row)
        {
            using __Cell = typename __Castmap::value_type::value_type;
            enum {
                _WIDTH = std::tuple_size<typename __Castmap::value_type>::value,
                _CASTMAP_CELL_UNDEFINED = (__Cell)_var_castmap_common<__Cell>::_CASTMAP_CELL_UNDEFINED
            };

            if( vid >= _castmap.size() )
                _castmap.resize(vid+1, make_array<__Cell, iseq_n<_WIDTH,_CASTMAP_CELL_UNDEFINED>>() );


            if( _castmap[vid][0] == _CASTMAP_CELL_UNDEFINED )
                init_row( _castmap[vid] );
        }


        template<typename __Castmap>
        auto *__new_castmap_row(__Castmap &_castmap, int vid)
        {
            using __Cell = typename __Castmap::value_type::value_type;
            enum {
                _WIDTH = std::tuple_size<typename __Castmap::value_type>::value,
                _CASTMAP_CELL_UNDEFINED = (__Cell)_var_castmap_common<__Cell>::_CASTMAP_CELL_UNDEFINED
            };

            if( vid >= _castmap.size() )
                _castmap.resize(vid+1, make_array<__Cell, iseq_n<_WIDTH,_CASTMAP_CELL_UNDEFINED>>() );

            if( _castmap[vid][0] == _CASTMAP_CELL_UNDEFINED ) {
                return &_castmap[vid];
            }

            return (decltype(&_castmap[vid]))nullptr;
        }




//___MI
template<typename __Domain, typename __BaseType, typename __CellType>
struct _VTypemap_base_castmap<__Domain, __BaseType, __CellType, true>
    : _var_castmap_common<__CellType>
{
    using Domain    = __Domain;
    using base_type = __BaseType;

    using castmap_cell_type = __CellType;
    using castmap_type = std::vector<std::array<castmap_cell_type, std::tuple_size<Domain>::value>>;

    castmap_type  _castmap;

    void update_castmap(int vid, const base_type *arg) {
        auto *row = __new_castmap_row(_castmap, vid);
        if( row )
            this->template __build_castmap_row<Domain>(*row, arg);
    }
};







////////////////////////////////////////////////////////////////////////////////////////
//poly
struct _VTypemap_poly_base_typemap
{
    using typemap_cell_type = int;

    enum {
        _TYPEMAP_CELL_UNDEFINED     =  (typemap_cell_type)0,

        _TYPEMAP_CELL_INDEX         =  (typemap_cell_type)0x80000000,
        _TYPEMAP_CELL_MULTI         =  (typemap_cell_type)0xc0000000,
        _TYPEMAP_CELL_MASK_INDEX    =  (typemap_cell_type)0x0000ffff,
        _TYPEMAP_CELL_MASK_MULTI    =  (typemap_cell_type)0x3fff0000,

        _TYPEMAP_CELL_INVALID       =  _TYPEMAP_CELL_INDEX,
    };


    static unsigned make_type_index (unsigned c) { assert((c&0xffff0000)==0); return  c      | _TYPEMAP_CELL_INDEX; }
    static unsigned make_multi_index(unsigned c) { assert((c&0xffff0000)==0); return (c<<16) | _TYPEMAP_CELL_MULTI; }

    static unsigned extract_type_index (unsigned c)  { return  c & _TYPEMAP_CELL_MASK_INDEX;        }
    static unsigned extract_multi_index(unsigned c)  { return (c & _TYPEMAP_CELL_MASK_MULTI) >> 16; }

};






template<typename __Domain, typename __BaseType, typename __CellType, bool __MI>
struct _VTM_base_vtype_MI
    : _VTypemap_vtype_typemap<__Domain, __BaseType,             __MI>
    , _VTypemap_base_castmap <__Domain, __BaseType, __CellType, __MI>
{
    template<typename BaseType>
    auto update(int vid, const BaseType *arg)
    {
        auto *castrow = __new_castmap_row(this->_castmap, vid);
        if( castrow )
            this->template __build_castmap_row<__Domain>(*castrow, arg);

        return this->template update_typemap(vid, arg, this->_castmap[vid]);
    }
};




/////////////////////////////////////////////////////////////////////////////////////
template<typename T>
struct __is_vtype_poly : std::integral_constant<bool, !std::is_base_of<vtype,T>::value && std::is_polymorphic<T>::value> { };




template<typename __Domain, typename __BaseType, bool __MI=false, bool __isPoly=false>
struct _vtypemap;



//virt_MI
template<typename __Domain, typename __BasePoly>
struct _vtypemap<__Domain, virtual_<__BasePoly>, true, false>
    : _VTM_base_vtype_MI<__Domain, virtual_<__BasePoly>, int, true>
{
private:
    using _Self  = _vtypemap;

public:
    using base_type = virtual_<__BasePoly>;
    using Domain    = __Domain;
    enum { __isMI = true };

    using castmap_cell_type = typename _VTypemap_base_castmap<__Domain, virtual_<__BasePoly>, int, true>::castmap_cell_type;

public:

    static void  *init() { return 0 ? &__instance : nullptr; }
    static _Self &get()  { return __instance; }


    int get_multi_index(const base_type *arg) const {
        return get()._get_multi_index( arg );
    }


    static auto get_type_index(const base_type *arg) {
        return get()._get_type_index( arg );
    }
    static auto get_type_index(const base_type &arg) {
        return get()._get_type_index( arg );
    }


    template<typename T>
    static T *cast(vtype::vtypeid_type vid, const base_type *__s) {

        return  (T*)((char*)__s + get()._castmap[vid][tuple_index<std::remove_const_t<T>,Domain>]);
    }


private:
    static _Self  __instance;
};

template<typename __Domain, typename __BasePoly>
_vtypemap<__Domain, virtual_<__BasePoly>, true, false>  _vtypemap<__Domain, virtual_<__BasePoly>, true, false>::__instance;





template<typename __Domain, typename __BaseType, typename __CellType, bool __MI>
struct _VTM_base_vtype_SI
    : _VTypemap_vtype_typemap<__Domain, __BaseType,             __MI>
    , _VTypemap_base_castmap <__Domain, __BaseType, __CellType, __MI>
{
    using castmap_cell_type = typename _VTypemap_base_castmap <__Domain, __BaseType, __CellType, __MI>::castmap_cell_type;

    template<typename BaseType>
    auto update(int vid, const BaseType *arg)
    {
        std::array<castmap_cell_type, std::tuple_size<__Domain>::value>  row;
        this->template __build_castmap_row<__Domain>(row, arg);
        this->template check_castmap_row(arg, row);
        return this->template update_typemap(vid, arg, row);
    }
};



//virt-SI
template<typename __Domain, typename __BasePoly>
struct _vtypemap<__Domain, virtual_<__BasePoly>, false, false>
    : _VTM_base_vtype_SI<__Domain, virtual_<__BasePoly>, int, false>
    , public __VType_base_caster<_vtypemap<__Domain, virtual_<__BasePoly>, false, false>, false>
{
private:
    using _Self  = _vtypemap<__Domain, virtual_<__BasePoly>, false, false>;

public:
    using base_type = virtual_<__BasePoly>;
    using base_type_poly = __BasePoly;
    using Domain    = __Domain;
    enum { __isMI = false };

    using typename _VTypemap_base_castmap<__Domain, base_type, int, false>::castmap_type;
    using __VType_base_caster<_vtypemap<__Domain, base_type, false, false>, false>::cast;


    static void *init() { return false ? &__instance : nullptr; }

    static _Self  &get() { return __instance; }

    static auto get_type_index(const base_type *arg) {
        assert( arg );
        return get()._get_type_index(arg);
    }
    static auto get_type_index(const base_type &arg) {
        return get()._get_type_index(arg);
    }


private: ////////////////////////////////////////////////////////////////////////
    static _Self  __instance;
};

template<typename __Domain, typename __BasePoly>
_vtypemap<__Domain, virtual_<__BasePoly>, false, false> _vtypemap<__Domain, virtual_<__BasePoly>, false, false>::__instance;





template<typename __D, size_t, typename, typename __Var>
struct ___Var_adjust_domain {
    using map = typename std::tuple<std::conditional_t<std::is_class<__D>::value, __D, typename __Var::template of<__D> >>;
};



//MI
template<typename __Domain, typename...__Tags>
struct _vtypemap<__Domain, var<__Tags...>, true, false>
    : _VTM_base_vtype_MI<type_map<__Domain, ___Var_adjust_domain, var<__Tags...>>, var<__Tags...>, int, true>
    , __VType_base_caster<_vtypemap<__Domain, var<__Tags...>, true, false>, true>
{
private:
    using _Self  = _vtypemap;

public:
    using base_type = var<__Tags...>;
    using Domain    = type_map<__Domain, ___Var_adjust_domain, base_type>;
    enum { __isMI = true };

    using castmap_cell_type = typename _VTypemap_base_castmap<Domain, var<__Tags...>, int, true>::castmap_cell_type;


    static int get_multi_index(const base_type *arg) {
        return get()._get_multi_index(arg);
    }

    static auto get_type_index(const base_type *arg) {
        return get()._get_type_index(arg);
    }
    static auto get_type_index(const base_type &arg) {
        return get()._get_type_index(arg);
    }


    static void *init() { return false ? &__instance : nullptr; }
    static _Self &get() { return __instance; }

private:
    static _Self  __instance;
};

template<typename __Domain, typename...__Tags>
_vtypemap<__Domain, var<__Tags...>, true, false> _vtypemap<__Domain, var<__Tags...>, true, false>::__instance;






//SI
template<typename __Domain, typename...__Tags>
struct _vtypemap<__Domain, var<__Tags...>, false, false>
    : _VTM_base_vtype_SI<type_map<__Domain, ___Var_adjust_domain, var<__Tags...>>, var<__Tags...>, int, false>
{
private:
    using _Self  = _vtypemap<__Domain, var<__Tags...>, false, false>;

public:
    using base_type = var<__Tags...>;
    using Domain    = type_map<__Domain, ___Var_adjust_domain, base_type>;
    enum { __isMI = false };


    static void  *init() { return false ? &__instance : nullptr; }
    static _Self &get()  { return __instance; }

    static auto get_type_index(const base_type *arg) {
        return get()._get_type_index(arg);
    }
    static auto get_type_index(const base_type &arg) {
        return get()._get_type_index(arg);
    }


private:
    static _Self  __instance;
};

template<typename __Domain, typename...__Tags>
_vtypemap<__Domain, var<__Tags...>, false, false> _vtypemap<__Domain, var<__Tags...>, false, false>::__instance;





template<typename __Domain, typename __BaseType, typename __CellType, bool __MI>
using _VTypemap_poly_base_castmap= _VTypemap_base_castmap<__Domain, __BaseType, __CellType, __MI>;



///////////////////////////////////////////////////////////////////////////////////////////////////////
template<typename __Domain, typename __BaseType, bool __MI=false, typename __MapType=std::unordered_map<const std::type_info*,unsigned>>
struct _vtypemap_poly;


//poly_SI//////////////////////////////////////////////////////////////////////////////////
template<typename __Domain, typename __BaseType, typename __MapType>
struct _vtypemap_poly<__Domain, __BaseType, false, __MapType>
    : _VTypemap_poly_base_typemap
    , _VTypemap_poly_base_castmap<__Domain, __BaseType, int, false>
{
public:
    __MapType   _typemap;
private:
    using _Self = _vtypemap_poly<__Domain, __BaseType, false, __MapType>;

public: 
    using Domain    = __Domain;
    using base_type = __BaseType;
    enum { __isMI = false };

    using _VTypemap_poly_base_typemap::extract_type_index;

    static auto get_type_index(const base_type *arg) {
        assert( arg );
        return extract_type_index(get()._typemap[&typeid(*arg)]);
    }
    static auto get_type_index(const base_type &arg) {
        return extract_type_index(get()._typemap[&typeid(arg)]);
    }

    static void *init() { return false ? &__instance : nullptr; }

    static _vtypemap_poly<__Domain, __BaseType, false>  &get() { return __instance; }


    template<typename CastmapRow>
    auto update_typemap(const std::type_info *vid, const base_type *arg, const CastmapRow &castmap)
    {
        static_assert( std::tuple_size<CastmapRow>::value == std::tuple_size<__Domain>::value, "");

        assert( vid == &typeid(*arg) );
        auto tix = this->_typemap[vid];

        if( tix == this->_TYPEMAP_CELL_UNDEFINED ) {
            static_assert( std::is_same<decltype(castmap[0]+0),int>::value, "");
            int bases[std::tuple_size<__Domain>::value];

            int bases_cnt = drtti::find_closest_bases_from_castmap<__Domain>(castmap, bases, this->_CASTMAP_CELL_INVALID);
            if( bases_cnt == 0 ) {
                _typemap[vid] = tix = this->_TYPEMAP_CELL_INVALID;
            }
            else if( bases_cnt == 1 ) {
                _typemap[vid] = tix = this->make_type_index( bases[0]+1 );
            }
            else {
                __throw_MI_detected();
            }
        }

        return tix;
    }

    auto update(const std::type_info *vid, const base_type *arg)
    {
        std::array<int, std::tuple_size<Domain>::value>  row;
        this->template __build_castmap_row<__Domain>(row, arg);
        this->template check_castmap_row(arg, row);
        return this->template update_typemap(vid, arg, row);
    }


    template<typename T>
    static T *cast(vtype::vtypeid_type vid, const base_type *__s) {
        static_assert( 0 <= tuple_index<T,Domain>, "argument type out of the domain");

        return vane::get<T>(const_cast<std::remove_const_t<base_type>*>(__s));
    }

private:
    static _vtypemap_poly<__Domain, __BaseType, false, __MapType>   __instance;

};

template<typename __Domain, typename __BaseType, typename __MapType>
_vtypemap_poly<__Domain, __BaseType, false, __MapType>  _vtypemap_poly<__Domain, __BaseType, false, __MapType>::__instance;




//////////////////////////////////////////////////////////////////////////////////
//poly_MI
template<typename __Domain, typename __BaseType, typename __MapType>
struct _vtypemap_poly<__Domain, __BaseType, true, __MapType>
    : _VTypemap_poly_base_typemap
    , _VTypemap_poly_base_castmap<__Domain, __BaseType, int, true>
{
    using Domain    = __Domain;
    using base_type = __BaseType;
    enum { __isMI = true };
private:
    using _Self = _vtypemap_poly;

public:
    using typename _VTypemap_poly_base_typemap::typemap_cell_type;
    std::vector<typemap_cell_type>  _typemap;
    __MapType                       _vtypemap;

protected:
    _vtypemap_poly() {
        assert( this == &get() );
        _typemap.push_back( this->_TYPEMAP_CELL_UNDEFINED );
        assert( _typemap.size() == 1 );
    }

public:
    using _VTypemap_poly_base_typemap::extract_type_index;

    static auto get_type_index(const base_type *arg) {
        assert( arg );

        return extract_type_index( get()._typemap[ get()._vtypemap[&typeid(*arg)] ] );
    }

    static auto get_type_index(const base_type &arg)
    {
        auto vid = get()._vtypemap[&typeid(arg)];
        return extract_type_index( get()._typemap[vid] );
    }
        static auto get_type_index(const base_type *arg, int &vid) {
            assert( arg );

            return extract_type_index( get()._typemap[ vid = get()._vtypemap[&typeid(*arg)] ] );
        }

        static auto get_type_index(const base_type &arg, int &vid) {
            return extract_type_index( get()._typemap[ vid = get()._vtypemap[&typeid(arg)] ] );
        }

    static void *init() { return false ? &__instance : nullptr; }

    static _vtypemap_poly<__Domain, __BaseType, true>   &get() { return __instance; }

    using _VTypemap_poly_base_castmap<__Domain, __BaseType, int, true>::update_castmap;


protected:
    template<typename CastmapRow>
    void _update_typemap(const CastmapRow &castmap)
    {
        static_assert( std::tuple_size<CastmapRow>::value == std::tuple_size<__Domain>::value, "");

        _Self::init();
        unsigned vid = _typemap.size() - 1;

        int bases[std::tuple_size<__Domain>::value];

        int bases_cnt = drtti::find_closest_bases_from_castmap<__Domain>(castmap, bases, this->_CASTMAP_CELL_INVALID);
        if( bases_cnt == 0 ) {
            _typemap.back() = this->_TYPEMAP_CELL_INVALID;
        }
        else if( bases_cnt == 1 ) {
            _typemap.back() = this->make_type_index(bases[0]+1);
        }
        else {
            _typemap.back() = this->make_multi_index(vid);
        }
    }

public:
    auto update(const base_type *arg)
    {
        _Self::init();
        auto type_info = &typeid(*arg);
        auto vid = _vtypemap[type_info];
        if( vid )
            return vid;

        vector_validate_at(_typemap, 0, (typemap_cell_type)this->_TYPEMAP_CELL_UNDEFINED);

        vid = _typemap.size();


        auto *castrow = __new_castmap_row(this->_castmap, vid);
        assert( castrow );
        this->template __build_castmap_row<Domain>(*castrow, arg);


        try {
            _typemap.push_back( this->_TYPEMAP_CELL_INVALID );
            _update_typemap(*castrow);
        }
        catch(...) {
            _typemap.pop_back();
            (*castrow)[0] = this->_CASTMAP_CELL_UNDEFINED;
            throw;
        }
        _vtypemap[type_info] = vid;

        return vid;
    }
    

    using _VTypemap_poly_base_castmap<__Domain, __BaseType, int, true>::_CASTMAP_CELL_INVALID;

    template<typename T>
    static T *cast(vtype::vtypeid_type vid, const base_type *__s) {
        assert( vid < get()._castmap.size() );
        return  (T*)((char*)__s + get()._castmap[vid][tuple_index<std::remove_const_t<T>,Domain>]);
    }

private:
    static _Self  __instance;

};

template<typename __Domain, typename __BaseType, typename __MapType>
_vtypemap_poly<__Domain, __BaseType, true, __MapType>   _vtypemap_poly<__Domain, __BaseType, true, __MapType>::__instance;




template<typename __Domain, typename __BaseType, bool __MI=false, bool __isPoly=false>
using vtypemap = _vtypemap<__Domain, std::remove_const_t<__BaseType>, __MI, __isPoly>;

template<typename __Domain, typename __BaseType, bool __MI=false, typename __MapType=std::unordered_map<const std::type_info*,unsigned>>
using vtypemap_poly = _vtypemap_poly<__Domain, std::remove_const_t<__BaseType>, __MI, __MapType>;


}//end-namespace vane_detail /////////////////////////////////////////////////////////////////////////////////////////



/*---------------------------------------------------------------------
    virtual signature
*/


template<typename...>
struct static_{};


namespace vane_detail {//////////////////////////////////////////////////////////////////////////

template<typename T>
struct _is_static_arg : std::integral_constant<bool,!basetype_is_polymorphic<T>::value> {
    using type = std::conditional_t< basetype_is_polymorphic<T>::value, std::tuple<>, std::tuple<T> >;
};
template<typename...T>
struct _is_static_arg<static_<T...>>        : std::true_type  { using type = std::tuple<T...>; };

template<typename T>
struct _remove_static_tag                   : std::false_type { using type = std::tuple<T>;     };
template<typename...T>
struct _remove_static_tag<static_<T...>>    : std::true_type  { using type = std::tuple<T...>;  };


template<typename Arg, size_t I, typename Tuple>
struct filter_static_arg : _is_static_arg<Arg> { 
    using map = typename _is_static_arg<Arg>::type;
};

template<typename Arg, size_t I, typename Tuple>
struct _mapper_remove_static_tag { using map = typename _remove_static_tag<Arg>::type; };

template<typename Args>
using _remove_static_tags = type_map<Args, _mapper_remove_static_tag>;




template<typename T>
struct _remove_virt_tag                 : std::false_type { using type = std::tuple<T>; };
template<typename T>
struct _remove_virt_tag<virtual_<T>>    : std::true_type  { using type = std::tuple<T>; };


template<typename Arg, size_t I, typename Tuple>
struct _mapper_remove_virt_tag { using map = typename _remove_virt_tag<Arg>::type; };

template<typename Args>
using _remove_virt_tags = type_map<Args, _mapper_remove_virt_tag>;


template<typename Arg, size_t I, typename Tuple>
struct _mapper_flag_virtual_arg { 
    using map = iseq_n< std::tuple_size< typename _remove_static_tag<Arg>::type >::value, !_is_static_arg<Arg>::value || basetype_is_base_of<__var,Arg>::value>;
};




template<typename Arg, size_t, typename>
struct _mapper_flag_polyarg { 
    using map = iseq_n<std::tuple_size< typename _remove_static_tag<Arg>::type >::value,
            !_is_static_arg<Arg>::value
        &&  !is_virt<remove_pointer_or_reference_t<Arg>>::value
        &&  !is_var <remove_pointer_or_reference_t<Arg>>::value
        >;
};


template<typename Arg, size_t I, typename Tuple>
struct _mapper_flag_virt { 
    using map = iseq_n<std::tuple_size< typename _remove_static_tag<Arg>::type >::value,
            !_is_static_arg<Arg>::value
        &&  is_virt<remove_pointer_or_reference_t<Arg>>::value
        >;
};

template<typename Arg, size_t I, typename Tuple>
struct _mapper_flag_var { 
    using map = iseq_n<std::tuple_size< typename _remove_static_tag<Arg>::type >::value,
            !_is_static_arg<Arg>::value
        &&  is_var<remove_pointer_or_reference_t<Arg>>::value
        >;
};

template<typename Arg, size_t I, typename Tuple>
struct _mapper_flag_const { 
    using map = iseq_n<std::tuple_size< typename _remove_static_tag<Arg>::type >::value,
            !_is_static_arg<Arg>::value
        &&  std::is_const<remove_pointer_or_reference_t<Arg>>::value
        >;
};




//////////////////////////////////////////////////////////
template<typename Arg>
struct __map_remove_base_const {
    using map = std::tuple< remove_base_const_t<Arg> >;
};

template<typename Arg, size_t, typename>
struct _filter_is_polyarg
    : std::__not_<std::__or_<
        is_virt<remove_pointer_or_reference_t<Arg>>,
        is_var <remove_pointer_or_reference_t<Arg>> 
       >>
    , __map_remove_base_const<Arg> { };

template<typename Arg, size_t I, typename>
struct _filter_is_virt
    : is_virt<remove_pointer_or_reference_t<Arg>>
    , __map_remove_base_const<Arg> { };

template<typename Arg, size_t, typename>
struct _filter_is_var
    : is_var<remove_pointer_or_reference_t<Arg>>
    , __map_remove_base_const<Arg> { };




enum { VSIG_ARGTYPE_NONV=0, VSIG_ARGTYPE_POLY, VSIG_ARGTYPE_VIRT, VSIG_ARGTYPE_VAR};
    template<typename Arg, size_t, typename>
    struct _mapper_ARGTYPE { 
        using map = iseq_n<std::tuple_size< typename _remove_static_tag<Arg>::type >::value,
                is_virt<remove_pointer_or_reference_t<Arg>>::value  ?  VSIG_ARGTYPE_VIRT :
                is_var <remove_pointer_or_reference_t<Arg>>::value  ?  VSIG_ARGTYPE_VAR  :
                ! _is_static_arg<Arg>::value                        ?  VSIG_ARGTYPE_POLY :
                                                                       VSIG_ARGTYPE_NONV
            >;
    };





//////////////////////////////////////////////////////////////////////////////////
template<typename _Sig>
struct resolve_virtual_signature {
public:

private:
public:
    using raw_arg_types   = typename resolve_signature<_Sig>::arg_types;

    using arg_types          = _remove_static_tags<raw_arg_types>;
    using static_arg_types   = typename type_map2<raw_arg_types, filter_static_arg>::type;
    using _virtual_arg_types_raw = typename type_map2<raw_arg_types, filter_static_arg>::complement;

    using return_type = typename resolve_signature<_Sig>::return_type;
    using _Return_type = std::conditional_t<std::is_same<return_type,void>::value, __void_return_t, return_type>;

    using plain_signature   = typename make_signature<return_type, arg_types>::type;
    using virtual_signature = _Sig;

    using polyarg_types = type_map<_virtual_arg_types_raw, _filter_is_polyarg>;
    using virt_types    = type_map<_virtual_arg_types_raw, _filter_is_virt>;
    using var_types     = type_map<_virtual_arg_types_raw, _filter_is_var>;

    using virtual_arg_types  = tuple_cat_t< virt_types, var_types, polyarg_types>;

    using virt_base_types =_remove_virt_tags< remove_pointers_or_references<virt_types>>;



    using virtual_flags   = type_mapv<raw_arg_types, _mapper_flag_virtual_arg>;
    using polyarg_flags   = type_mapv<raw_arg_types, _mapper_flag_polyarg>;
    using virt_flags      = type_mapv<raw_arg_types, _mapper_flag_virt>;
    using var_flags       = type_mapv<raw_arg_types, _mapper_flag_var>;
    using const_flags     = type_mapv<raw_arg_types, _mapper_flag_const>;


    using NARGTYPEs        = iseq_cat_t<
        iseq_n<std::tuple_size<virt_types   >::value, VSIG_ARGTYPE_VIRT>,
        iseq_n<std::tuple_size< var_types   >::value, VSIG_ARGTYPE_VAR>,
        iseq_n<std::tuple_size<polyarg_types>::value, VSIG_ARGTYPE_POLY>
    >;


    enum {
        ARGC  = std::tuple_size<arg_types>::value,
        VARGC = std::tuple_size<virtual_arg_types>::value,
        ARGC_POLY = std::tuple_size<polyarg_types>::value,
    };

    using index_from_normal = iseq_cat_t< 
        iseq_map<virt_flags,    iseq_filters::filter_nonZero_into_index>,
        iseq_map< var_flags,    iseq_filters::filter_nonZero_into_index>,
        iseq_map<polyarg_flags, iseq_filters::filter_nonZero_into_index>,
        iseq_map<virtual_flags, iseq_filters::filter_value_into_index, std::integral_constant<int,0>>
    >;


    using index_to_normal = iseq_inverse< index_from_normal >;
    using normalized_arg_types = tuple_elements<index_from_normal, arg_types>;
};





template<bool P, typename T>
struct add_const_if {
    using type = std::conditional_t<P, std::add_const_t<T>, T>;
};

template<typename X, size_t I, typename Tuple, typename Virtual_indices, typename ToCastArgs>
struct _mapper_decorate_virtual_arg {
    enum { J = iseq_at<I,Virtual_indices> };
    using _ToCast = std::tuple_element_t<( (J >= std::tuple_size<ToCastArgs>::value) ? 0 : J), ToCastArgs>;
    using ToCast = typename add_const_if<std::is_const<typename remove_pointer_or_reference<X>::type>::value,_ToCast >::type;
    using map = std::tuple<
                    std::conditional_t< (J >= std::tuple_size<ToCastArgs>::value),  X,
                    std::conditional_t< std::is_pointer<X>::value,          std::add_pointer_t<ToCast>,
                    std::conditional_t< std::is_lvalue_reference<X>::value, std::add_lvalue_reference_t<ToCast>,
                    std::conditional_t< std::is_rvalue_reference<X>::value, std::add_rvalue_reference_t<ToCast>,
                                                                            ToCast
                    >>>>    
                >;
};

template<typename VSig, typename ToCastArgs>
struct substitute_virtual_args {
    using type = 
        type_map<
            typename VSig::arg_types,
            _mapper_decorate_virtual_arg, 
            typename VSig::index_to_normal,
            ToCastArgs
        >;
};



/*---------------------------------------------------------------------
    arg_sig
*/

struct ArgSigTraits {
    template<typename Args, typename Sig, typename=std::__void_t<>>
    struct is_callable: std::false_type {
        using type = std::tuple<>;
    };
    template<typename...ArgI, typename...ParamI>
    struct is_callable<std::tuple<ArgI...>, std::tuple<ParamI...>, std::__void_t<decltype( std::declval<void(*)(ParamI...)>()( std::declval<ArgI>()...) )> > : std::true_type {
        using type = std::tuple< std::tuple<ParamI...> >;
    };
};

struct PolyArgSigTraits {
    template<typename Args, typename Sig, typename=std::__void_t<>>
    struct is_callable: std::false_type {
        using type = std::tuple<>;
    };
    template<typename...ArgI, typename...ParamI>
    struct is_callable<std::tuple<ArgI...>, std::tuple<ParamI...>, std::__void_t<decltype( std::declval<void(*)(ParamI*...)>()( std::declval<ArgI*>()...) )> > : std::true_type {
        using type = std::tuple< std::tuple<ParamI...> >;
    };
};





/*---------------------------------------------------------------------
    argdef
*/


template<typename T, size_t, typename>
struct map_topology_sorted {
    using map = std::tuple<typename topological_sort<T>::type>;
};

        template<typename T, size_t=0, typename=void>
        struct is_domain_MI : std::integral_constant<bool, topological_sort<T>::isMI> { };

        template<typename Domains>
        constexpr bool domains_multi_inherited = tuple_any_of<Domains, is_domain_MI>;


template<typename Tuple>
using __strip_virtual_args = remove_consts<remove_pointers_or_references<Tuple>>;


template<typename Domain, size_t I, typename Domains, typename BaseType, typename BaseTypes>
struct _ArgDef_filter_sameBase : std::is_base_of<BaseType, typename std::tuple_element<I,BaseTypes>::type> { };


template<typename T, size_t, typename>
struct _map_virt_base : std::true_type {
    using map = std::conditional_t< is_virt<T>::value, std::tuple<__virt>, std::tuple<T> >;
};
template<typename T, size_t, typename>
struct _filter_virt_base : is_virt<T> { };

template<typename V, size_t, typename>
struct _map_varg_vartypes {
    using map = std::tuple<typename V::Virtuals>;
};

template <typename D, typename B>
struct is_valid_domain : std::true_type {
    static_assert( is_base_of_all<B,D>, "vane multi-function: domain error: a polymorphic virtual argument must be a subclass of the corresponding argument type of the declared function type in FX" );
};

template <typename D, typename B>
struct is_valid_domain<D,virtual_<B>> : std::true_type {
    static_assert( is_base_of_all<typename virtual_<B>::base_type_poly,D>, "vane multi-func: domain error: in virtual_<Base>::of<T>, T must be derived from Base" );
};

template <typename D, typename...Tags>
struct is_valid_domain<D,var<Tags...>> : std::true_type {
};

template <typename D, size_t I, typename, typename Bs>
struct filter_valid_domain : is_valid_domain<D,typename std::tuple_element<I,Bs>::type> { 
    static_assert(!std::is_same<D,std::tuple<>>::value, "vane multi-function: domain error: empty domain");
};


template<typename T,size_t,typename>
struct _ArgDef_filter_is_poly :  std::integral_constant<bool, !std::is_base_of<vtype,T>::value> {};



template <typename _VSig, typename __Domains, typename _Domains=type_map<__Domains, map_topology_sorted>>
struct __ArgDef__
{
    using VSig  = _VSig;
    enum { ARGC = VSig::ARGC };
private:
    using _VSig_vo = resolve_virtual_signature<typename make_signature<void, typename VSig::_virtual_arg_types_raw>::type>;

public:
    using Domains = tuple_elements<typename _VSig_vo::index_from_normal, _Domains>;

    using _BaseTypes_all = __strip_virtual_args< typename VSig::virtual_arg_types >;
    using BaseTypes_virt = type_map<_BaseTypes_all, _filter_virt_base>;
    using BaseTypes     = _BaseTypes_all;
    using basetypes_var  = __strip_virtual_args< typename VSig::var_types>;


    static_assert( std::tuple_size<Domains>::value == std::tuple_size<typename VSig::virtual_arg_types>::value, "invalid domain set: size mismatch" );
    static_assert( tuple_all_of<Domains, filter_valid_domain, _BaseTypes_all>, "invalid domain");
};





/*---------------------------------------------------------------------
    arg_caster
*/

template<typename T, typename S, typename=void>
struct is_static_castable : std::false_type {};

template<typename T, typename S>
struct is_static_castable<T,S,std::__void_t<decltype( static_cast<remove_pointer_or_reference_t<T>*>((remove_pointer_or_reference_t<S>*)nullptr)  )>> : std::true_type{ };




struct _vtype_category_poly;

template<typename T>
struct _VType_category {
    using type = _vtype_category_poly;
};

template<typename Base>
struct _VType_category<virtual_<Base>> {
    using type = __virt;
};

template<typename...Tags>
struct _VType_category<var<Tags...>> {
    using type = __var;
};

template<typename T>
using _vtype_category_t = typename _VType_category<std::remove_const_t<T>>::type;



template<typename T, typename S, typename MF,
        bool __isMI=MF::__isMI,
        bool __isToClass = std::is_class<remove_pointer_or_reference_t<T>>::value,
        typename __VTCat = _vtype_category_t<remove_pointer_or_reference_t<S>>, typename=void>
struct var_caster;


template<typename T, typename S, typename __MF, bool __MI, typename Dummy>
class var_caster<T, S, __MF, __MI, true, _vtype_category_poly, Dummy>
{
    static_assert(__MI==true, "");

    using TT = remove_pointer_or_reference_t<T>;
    using SS = remove_pointer_or_reference_t<S>;
public:


    template<size_t __I>
    static
    std::conditional_t<std::is_rvalue_reference<S>::value,T,T&>
    forward(SS &__s)
    {
        using Domain  = typename std::tuple_element<__I,typename __MF::domains>::type;
        using TypeMap = vtypemap_poly<Domain, SS, __MI>;

        return static_cast<std::conditional_t<std::is_rvalue_reference<S>::value,T,T&>>(
                    *TypeMap::template cast<TT>(TypeMap::get()._vtypemap[&typeid(__s)], &__s));
    }


    template<size_t __I>
    static
    TT*
    forward(SS *__s)
    {
        using Domain  = typename std::tuple_element<__I,typename __MF::domains>::type;
        using TypeMap = vtypemap_poly<Domain, SS, __MI>;

        return TypeMap::template cast<TT>(TypeMap::get()._vtypemap[&typeid(*__s)], __s);
    }
};



template<typename T, typename S, typename __MF, bool __MI>
class var_caster<T, S, __MF, __MI, true, _vtype_category_poly, std::__void_t<decltype(static_cast<remove_pointer_or_reference_t<T>*>((std::remove_const_t<remove_pointer_or_reference_t<S>>*)nullptr))> >
{
    using TT = remove_pointer_or_reference_t<T>;
    using SS = remove_pointer_or_reference_t<S>;
public:

    template<size_t __I>
    static
    std::conditional_t<std::is_rvalue_reference<S>::value,T,T&>
    forward(SS &__s) noexcept
    {
        return static_cast<std::conditional_t<std::is_rvalue_reference<S>::value, T, T&>>(const_cast<remove_const_t<SS>&>(__s));
    }

    template<size_t __I>
    static
    TT*
    forward(SS *__s) noexcept
    {
        return static_cast<TT*>(const_cast<std::remove_const_t<SS>*>(__s));
    }
};



template<typename T, typename S, typename __MF, bool __MI, bool __IsToClass, typename Dummy>
class var_caster<T, S, __MF, __MI, __IsToClass, __virt, Dummy> {
    static_assert( __MI == true, "needs multi_func<...,true>");

    using TT = remove_pointer_or_reference_t<T>;
    using SS = remove_pointer_or_reference_t<S>;
public:

    template<size_t __I>
    static
    std::conditional_t<std::is_rvalue_reference<S>::value,T,T&>
    forward(SS &__s)
    {
        using Domain  = typename std::tuple_element<__I,typename __MF::domains>::type;
        using TypeMap = vtypemap<Domain, SS, __MI>;

        TypeMap::init();

        return static_cast<std::conditional_t<std::is_rvalue_reference<S>::value, T, T&>>(*TypeMap::template cast<TT>(__s.vtypeid(), &__s));
    }

    template<size_t __I>
    static
    TT*
    forward(SS *__s) noexcept
    {
        using Domain  = typename std::tuple_element<__I,typename __MF::domains>::type;
        using TypeMap = vtypemap<Domain, SS, __MI>;

        TypeMap::init();

        return TypeMap::template cast<TT>(__s->vtypeid(), __s);
    }
};


template<typename T, typename S, typename __MF, bool __isMI, bool __IsToClass>
class var_caster<T, S, __MF, __isMI, __IsToClass, __virt, std::__void_t<decltype(static_cast<remove_pointer_or_reference_t<T>*>((typename std::remove_const<typename remove_pointer_or_reference<S>::type>::type::base_type_poly*)nullptr))>>
{
    using TT = remove_pointer_or_reference_t<T>;
    using SS = remove_pointer_or_reference_t<S>;
public:

    template<size_t __I>
    static
    std::conditional_t<std::is_rvalue_reference<S>::value,T,T&>
    forward(SS &__s)
    {
        using Domain  = typename std::tuple_element<__I,typename __MF::domains>::type;
        using TypeMap = vtypemap<Domain, SS, __isMI>;

        TypeMap::init();

        if( __isMI )
            return static_cast<std::conditional_t<std::is_rvalue_reference<S>::value, T, T&>>(*vane::get<TT>(
                static_cast<TT*>((typename SS::base_type_poly*)((char*)&__s + __s._vdiff))
            ));
        else 
            return static_cast<std::conditional_t<std::is_rvalue_reference<S>::value, T, T&>>(*vane::get<TT>(remove_const_t<SS*>(&__s)));
    }


    template<size_t __I>
    static
    TT*
    forward(SS *__s) noexcept
    {
        using Domain  = typename std::tuple_element<__I,typename __MF::domains>::type;
        using TypeMap = vtypemap<Domain, SS, __isMI>;

        TypeMap::init();

        if( __isMI )
            return static_cast<TT*>((typename SS::base_type_poly*)((char*)__s + __s->_vdiff));
        else {
            return vane::get<TT>(const_cast<std::remove_const_t<SS>*>(__s));
        }
    }
};




//var<>
template<typename T, typename S, typename __MF, bool __isMI>
class var_caster<T, S, __MF, __isMI, false,  __var>
{
    using TT = remove_pointer_or_reference_t<T>;
    using SS = remove_pointer_or_reference_t<S>;
public:

    template<size_t __I>
    static
    std::conditional_t<std::is_rvalue_reference<S>::value,T,T&>
    forward(SS &__s)
    {
        return static_cast<std::conditional_t<std::is_rvalue_reference<S>::value, T, T&>>(vane::get<TT>(const_cast<std::remove_const_t<SS>&>(__s)));
    }

    template<size_t __I>
    static
    TT*
    forward(SS *__s) noexcept {
        return vane::get<TT>(const_cast<std::remove_const_t<SS>*>(__s));
    }

};


template<typename T, typename S, typename __MF>
class var_caster<T, S, __MF, false, true, __var>
{
    using TT = remove_pointer_or_reference_t<T>;
    using SS = remove_pointer_or_reference_t<S>;
public:

    template<size_t __I>
    static
    TT*
    forward(SS *__s) noexcept
    {
        using Domain  = typename std::tuple_element<__I,typename __MF::domains>::type;
        vtypemap<Domain, SS, false>::init();

        return vane::get<TT>(const_cast<std::remove_const_t<SS>*>(__s));
    }

    template<size_t __I>
    static
    std::conditional_t<std::is_rvalue_reference<S>::value,T,T&>
    forward(SS &__ss)
    {
        remove_const_t<SS> &__s = const_cast<remove_const_t<SS>&>(__ss);

        using Domain  = typename std::tuple_element<__I,typename __MF::domains>::type;
        vtypemap<Domain, SS, false>::init();

        return static_cast<std::conditional_t<std::is_rvalue_reference<S>::value, T, T&>>(vane::get<TT>(__s));
    }
};


template<typename T, typename S, typename __MF>
class var_caster<T, S, __MF, true, true, __var> 
{
    using TT = remove_pointer_or_reference_t<T>;
    using SS = remove_pointer_or_reference_t<S>;

public:
    template<size_t __I>
    static
    std::conditional_t<std::is_rvalue_reference<S>::value,T,T&>
    forward(SS &__s)
    {
        using Domain    = typename std::tuple_element<__I,typename __MF::domains>::type;
        static_assert( 0 <= tuple_index<TT,Domain>, "argument type out of the domain");

        return static_cast<std::conditional_t<std::is_rvalue_reference<S>::value, T, T&>>(*vtypemap<Domain, SS, true>::template cast<TT>(__s.vtypeid(), &__s));
    }

    template<size_t __I>
    static
    TT*
    forward(SS *__s) noexcept
    {
        using Domain    = typename std::tuple_element<__I,typename __MF::domains>::type;
        static_assert( 0 <= tuple_index<std::remove_const_t<TT>,Domain>, "argument type out of the domain");

        return vtypemap<Domain, SS, true>::template cast<TT>(__s->vtypeid(), __s);
    }
};



template<typename T, typename S, typename MF>
struct arg_caster
{
    template<size_t __I>
    static
    constexpr S&&
    forward(typename std::remove_reference<S>::type &__s) noexcept
    {
        return std::forward<S&&>(__s);
    }

    template<size_t __I>
    static
    constexpr S&&
    forward(typename std::remove_reference<S>::type &&__s) noexcept
    {
        return std::forward<S&&>(__s);
    }
};




//  utils
template<typename T> inline
auto &__base_typeid(T &&x) { return typeid(x); }

template<typename T> inline
auto &__base_typeid(T *x) { return typeid(*x); }




/*---------------------------------------------------------------------
    _Hash
*/

template<typename T>
struct _Hash;

template<typename T,size_t N>
struct _Hash<std::array<T,N>> : public std::__hash_base<size_t, std::array<T,N>>
{
    size_t operator()(const std::array<T,N> & a) const noexcept {
        return std::_Hash_impl::hash(a.data(), a.size());
    }
};


template<typename T>
T *get_base_ptr(T *p) {
    return p;
}

template<typename T>
T *get_base_ptr(T &ref) { 
    return &ref;
}




template<typename T, size_t I, typename Tuple>
struct map_tuple_wrapped { using map = std::tuple< std::tuple<T> >; };

struct _VT_CHECK{};
template<typename MF, typename ToCastArgs,
    typename BaseArgs = typename MF::BaseTypes, 
    typename SigArgs  = typename MF::VSig::arg_types,
    typename RealArgs = remove_consts<typename substitute_virtual_args<typename MF::VSig, ToCastArgs>::type>,
    typename ArgISeq  = std::make_index_sequence<MF::ARGC>,
    typename =  std::conditional_t< tuple_index<_VT_CHECK,ToCastArgs,false> >= 0,                       _VT_CHECK,
                std::conditional_t< FX_utils::__FX_exists_callable<typename MF::FX, RealArgs>::value,   std::true_type,
                                                                                                        std::false_type >
                >
> struct _CallerHelper;

template<typename MF, typename...Ta, typename...BaseArg, typename...Sa, typename...Ra, size_t..._AIs>
struct _CallerHelper<MF, std::tuple<Ta...>, std::tuple<BaseArg...>, std::tuple<Sa...>, std::tuple<Ra...>, std::index_sequence<_AIs...>, _VT_CHECK>
{
    static constexpr
    typename MF::VFunc get_caller() {
        return MF::_check_error;
    }
};

template<typename MF, typename...Ta, typename...BaseArg, typename...Sa, typename...Ra, size_t..._AIs>
struct _CallerHelper<MF, std::tuple<Ta...>, std::tuple<BaseArg...>, std::tuple<Sa...>, std::tuple<Ra...>, std::index_sequence<_AIs...>, std::false_type>
{
    static constexpr
    typename MF::VFunc get_caller() {
        return &MF::__error_NO_MATCH;
    }
};

template<typename MF, typename...Ta, typename...BaseArg, typename...Sa, typename...Ra, size_t..._AIs>
struct _CallerHelper<MF, std::tuple<Ta...>, std::tuple<BaseArg...>, std::tuple<Sa...>, std::tuple<Ra...>, std::index_sequence<_AIs...>, std::true_type>
{
    using VSig = typename MF::VSig;
    using _Return_type = typename VSig::_Return_type;

    static _Return_type call(typename MF::FX *fx, Sa...args)
    {
        return FxCaller<typename VSig::return_type,MF>::call( fx,
                            std::conditional_t< iseq_at<_AIs,typename MF::VSig::virtual_flags>, var_caster<Ra,Sa,MF>, arg_caster<Ra,Sa,MF> >
                                ::template forward<iseq_at<_AIs, typename VSig::index_to_normal>>(args)... );
    }
    static constexpr
    typename MF::VFunc get_caller() {
        return &call;
    }
};




template<typename MF, typename...DimTs>
struct _VTableBuilderHelper;

template<typename MF, typename...Ts>
struct _VTableBuilderHelper<MF, std::tuple<Ts...>> {
    using table_type = std::array<typename MF::VFunc, sizeof...(Ts)>;

    template <typename Ta=std::tuple<>>
    static constexpr
    table_type get_table() {
        return table_type{ _CallerHelper<MF, tuple_add_t<Ta,Ts>>::get_caller()... };
    }
};

template<typename MF, typename...Ts, typename...Ds>
struct _VTableBuilderHelper<MF, std::tuple<Ts...>,Ds...> {
    using Prev = _VTableBuilderHelper<MF,Ds...>;
    using table_type = std::array< typename Prev::table_type, std::tuple_size<std::tuple<Ts...>>::value >;

    template <typename Ta = std::tuple<>>
    static constexpr
    table_type get_table() {
        return  table_type{ Prev::template get_table<tuple_add_t<Ta,Ts>>()...  };
    }
};

template<typename T, size_t, typename>
struct __TB_map_domain {
    using map=std::tuple<tuple_cat_t<std::tuple<_VT_CHECK>, T>>;
};

template<typename MF>
struct __TB_adjust_domains {
    using type = type_map<typename MF::domains, __TB_map_domain>;
};

template<typename MF, typename Domains=typename __TB_adjust_domains<MF>::type >
struct __VTableBuilder;

template<typename MF, typename...Ds>
struct __VTableBuilder<MF, std::tuple<Ds...>>
{
    using table_type = typename _VTableBuilderHelper<MF, Ds...>::table_type;

    static
    constexpr table_type get_table() {
        return _VTableBuilderHelper< MF, Ds...>::get_table();
    }
};


template<typename MF, typename Domain,
    typename Args = typename substitute_virtual_args<typename MF::VSig, Domain>::type >
struct _SigTableHelper : FX_utils::FX_exists_sig<typename MF::FX, Args>
{ };


template<typename MF, typename...DimTs>
struct _SigTableBuilderHelper;

template<typename MF, typename...Ts>
struct _SigTableBuilderHelper<MF, std::tuple<Ts...>> {
    static_assert( sizeof(bool)==1, "");
    using table_type = std::array<bool, sizeof...(Ts)>;

    template <typename Ta=std::tuple<>>
    static constexpr
    table_type get_table() {
        return table_type{ _SigTableHelper<MF, tuple_add_t<Ta,Ts>>::value... };
    }
};

template<typename MF, typename...Ts, typename...Ds>
struct _SigTableBuilderHelper<MF, std::tuple<Ts...>,Ds...> {
    using Prev = _SigTableBuilderHelper<MF,Ds...>;
    using table_type = std::array< typename Prev::table_type, std::tuple_size<std::tuple<Ts...>>::value >;

    template <typename Ta = std::tuple<>>
    static constexpr
    table_type get_table() {
        return  table_type{ Prev::template get_table<tuple_add_t<Ta,Ts>>()...  };
    }
};

template<typename MF, typename Domains, bool=MF::__isMI>
struct __SigTableBuilder;

template<typename MF, typename...Ds>
struct __SigTableBuilder<MF, std::tuple<Ds...>, true>
{
    using table_type = typename _SigTableBuilderHelper<MF, Ds...>::table_type;

    static
    constexpr table_type get_table() {
        return _SigTableBuilderHelper< MF, Ds...>::get_table();
    }
};

template<typename MF, typename...Ds>
struct __SigTableBuilder<MF, std::tuple<Ds...>, false>
{
    using table_type = std::array<bool,0>;

    static
    constexpr table_type get_table() {
        return table_type{};
    }
};




template<
    typename _Sig, 
    typename _Args=typename resolve_virtual_signature<_Sig>::arg_types, 
    typename _ArgSeq=std::make_index_sequence<std::tuple_size<_Args>::value>, 
    typename _VArgSeq=std::make_index_sequence<std::tuple_size< typename resolve_virtual_signature<_Sig>::virtual_arg_types >::value>,
    typename _NormalizedArgs = typename resolve_virtual_signature<_Sig>::normalized_arg_types,
    typename _return_type = typename resolve_virtual_signature<_Sig>::return_type
    >
struct virtual_func_nonVf;


template<typename _Sig, typename..._Args, size_t..._AIs, size_t..._VIs, typename..._NArgs, typename _return_type >
struct virtual_func_nonVf<_Sig, std::tuple<_Args...>, std::index_sequence<_AIs...>, std::index_sequence<_VIs...>, std::tuple<_NArgs...>,_return_type> {

protected:
    enum { ARGC = sizeof...(_Args) };
    enum { VARGC = sizeof...(_VIs) };

    using VSig = resolve_virtual_signature<_Sig>;

    using _Return_type = typename VSig::_Return_type;
    using arg_types   = typename VSig::arg_types;

    using virtual_arg_types = typename VSig::virtual_arg_types;
    using static_arg_types  = typename VSig::static_arg_types;
    using index_from_normal = typename VSig::index_from_normal;

    using Self = virtual_func_nonVf;

    _Return_type (*_vfunc)(Self*,_NArgs...);
    typedef _Return_type (*_vfunc_type)(Self*,_NArgs...);


    virtual_func_nonVf(_Return_type (*vfunc)(Self*,_NArgs...)=nullptr) : _vfunc(vfunc) {}

public:
    using type = _Sig;

    _Return_type operator()(_Args...args) {
        typename VSig::arg_types    __args{std::forward<_Args>(args)...};

        return _vfunc(this, std::forward<_NArgs>(std::get<iseq_at<_AIs, index_from_normal>>(__args))... );
    }

    _Return_type call(_Args...args) {
        typename VSig::arg_types  __args{std::forward<_Args>(args)...};

        return _vfunc(this, std::forward<_NArgs>(std::get<iseq_at<_AIs, index_from_normal>>(__args))... );
    }
};



template<typename _Sig, typename..._Args, size_t..._AIs, size_t..._VIs, typename..._NArgs>
struct virtual_func_nonVf<_Sig, std::tuple<_Args...>, std::index_sequence<_AIs...>, std::index_sequence<_VIs...>, std::tuple<_NArgs...>,void> {
protected:
    enum { ARGC = sizeof...(_Args) };
    enum { VARGC = sizeof...(_VIs) };

    using VSig = resolve_virtual_signature<_Sig>;

    using _Return_type = typename VSig::_Return_type;
    using arg_types   = typename VSig::arg_types;

    using virtual_arg_types = typename VSig::virtual_arg_types;
    using static_arg_types  = typename VSig::static_arg_types;
    using index_from_normal = typename VSig::index_from_normal;

    using Self = virtual_func_nonVf;

    _Return_type (*_vfunc)(Self*,_NArgs...);

    virtual_func_nonVf(_Return_type (*vfunc)(Self*,_NArgs...)=nullptr) : _vfunc(vfunc) {}

public:
    using type = _Sig;

    void operator()(_Args...args) {
        typename VSig::arg_types    __args{std::forward<_Args>(args)...};

        _vfunc(this, std::forward<_NArgs>(std::get<iseq_at<_AIs, index_from_normal>>(__args))... );
    }

    void call(_Args...args) {
        typename VSig::arg_types  __args{std::forward<_Args>(args)...};

        _vfunc(this, std::forward<_NArgs>(std::get<iseq_at<_AIs, index_from_normal>>(__args))... );
    }
};






template<typename FX,
        bool MI,
        template <typename...> class Map = std::unordered_map,
        typename Args=typename resolve_virtual_signature<typename FX::type>::arg_types, 
        typename _ArgSeq=std::make_index_sequence<std::tuple_size<Args>::value>,
        typename _VArgSeq=std::make_index_sequence<std::tuple_size< typename resolve_virtual_signature<typename FX::type>::virtual_arg_types >::value>,
        typename _NormalizedArgs = typename resolve_virtual_signature<typename FX::type>::normalized_arg_types
        >
struct multi_func_TM;


template<typename _FX, bool _MI, template <typename...> class Map, typename..._Args, size_t..._AIs, size_t..._VIs, typename..._NArgs>
struct multi_func_TM<_FX,_MI, Map, std::tuple<_Args...>, std::index_sequence<_AIs...>, std::index_sequence<_VIs...>, std::tuple<_NArgs...>>
    : virtual_func_nonVf<typename _FX::type> ,_FX
{
    enum {  __isMI = _MI };
    using FX = _FX;
protected:
    using Self = multi_func_TM;

    using Super =  virtual_func_nonVf<typename FX::type>;
    using Super::operator();
    using Super::call;
public:

    using VSig = typename Super::VSig;
    using typename Super::_Return_type;
    using typename Super::virtual_arg_types;
    using typename Super::static_arg_types;


    enum {  ARGC        = Super::ARGC };
    enum {  VARGC       = Super::VARGC };
    enum {  ARGC_POLY   = VSig::ARGC_POLY };



    using type  = typename _FX::type;
    using VFunc = typename make_signature<typename VSig::_Return_type, tuple_cat_t<std::tuple<FX*>, typename VSig::arg_types>>::type*;

    using _ArgDef = __ArgDef__<VSig, typename FX::domains>;


    using domains           = typename _ArgDef::Domains;
    using BaseTypes         = typename _ArgDef::BaseTypes;


    using VTableBuilder = __VTableBuilder<Self>;
    using vtable_type   = typename VTableBuilder::table_type;

    using SigTableBuilder = __SigTableBuilder<Self, domains>;
    using sigtable_type   = typename SigTableBuilder::table_type;

    static_assert(__isMI || !domains_multi_inherited<domains>, "multiple-inheritance detected");



public:
    static
    typename VSig::_Return_type __error_OOD(FX *fx, _Args...args ) {
        return _Error_OOD_Helper<Self>::call(fx, std::forward<_Args>(args)...);
    }

    static
    typename VSig::_Return_type __error_NO_MATCH(FX *fx, _Args...args ) {
        return _Error_NO_MATCH_Helper<Self>::call(fx, std::forward<_Args>(args)...);
    }

    static 
    typename VSig::_Return_type __error_OOD_default(FX *fx, _Args...args ) {
        __throw_OOD();
        return typename VSig::_Return_type();
    }

    static
    typename VSig::_Return_type __error_NO_MATCH_default(FX *fx, _Args...args ) {
        __throw_NO_MATCH();
        return typename VSig::_Return_type();
    }


    using Super::_vfunc;

    template<typename...Args>
    multi_func_TM(Args&&...args) : Super(__vcall), _FX{std::forward<Args>(args)...} { }





protected:

    using typename Super::index_from_normal;
    using normalized_arg_types = typename VSig::normalized_arg_types;


    //vtype's
    template<size_t I, int ARGTYPE=iseq_at<I,typename VSig::NARGTYPEs>, bool isMI=__isMI>
    struct _Index_fetcher {
        static int get(const std::tuple<_NArgs...>&__args, int&)
        {
            using base_type = typename std::tuple_element<I,BaseTypes>::type;
            vtypemap<typename std::tuple_element<I,domains>::type, base_type, __isMI>::init();

            return vtypemap<typename std::tuple_element<I,domains>::type, base_type, __isMI>
                    ::get_type_index(std::get<I>(__args));

        }
    };

    //poly
    template<size_t I>
    struct _Index_fetcher<I, VSIG_ARGTYPE_POLY, false> {
        static int get(const std::tuple<_NArgs...>&__args, int &)
        {
            return vtypemap_poly<typename std::tuple_element<I,domains>::type, typename std::tuple_element<I,BaseTypes>::type, __isMI>
                    ::get_type_index(std::get<I>(__args));
        }
    };

    template<size_t I>
    struct _Index_fetcher<I, VSIG_ARGTYPE_POLY, true> {
        static int get(const std::tuple<_NArgs...>&__args, int &vid)
        {
            return vtypemap_poly<typename std::tuple_element<I,domains>::type, typename std::tuple_element<I,BaseTypes>::type, __isMI>
                    ::get_type_index(std::get<I>(__args), vid);
        }
    };

public:

    static const auto &__get_vtypemaps() {
        static auto __vtypemaps = std::make_tuple(
            &std::conditional_t<iseq_at<_VIs,typename VSig::NARGTYPEs> == VSIG_ARGTYPE_POLY,
                vtypemap_poly<typename std::tuple_element<_VIs,domains>::type, typename std::tuple_element<_VIs,BaseTypes>::type, __isMI>,
                vtypemap<typename std::tuple_element<_VIs,domains>::type, typename std::tuple_element<_VIs,BaseTypes>::type, __isMI>
            >::get()...
        );
        return __vtypemaps;
    }

    static auto __get_castmaps(const std::array<int,VARGC> &vids, std::array<int,VARGC> *lengths = nullptr) {
        return std::array<const int*, VARGC> { 
            &std::conditional_t<iseq_at<_VIs,typename VSig::NARGTYPEs> == VSIG_ARGTYPE_POLY,
                vtypemap_poly<typename std::tuple_element<_VIs,domains>::type, typename std::tuple_element<_VIs,BaseTypes>::type, __isMI>,
                vtypemap<typename std::tuple_element<_VIs,domains>::type, typename std::tuple_element<_VIs,BaseTypes>::type, __isMI>
            >::get()._castmap[vids[_VIs]][0]...
        };
    }


public:
    _Return_type _call(_Args...args)
    {
        std::tuple<_Args...>  __args{std::forward<_Args>(args)...};
        return __vcall(this, std::forward<_NArgs>(std::get<iseq_at<_AIs, index_from_normal>>(__args))... );
    }

protected:
    static _Return_type
    __vcall(Super *__this, _NArgs...args)
    {
#ifndef NDEBUG
        __check_mf_init();
#endif

        std::tuple<_NArgs...>   __args{std::forward<_NArgs>(args)...};
        enum { use_cache = __isMI && (ARGC_POLY>0) };
        int vid[use_cache ? VARGC : 0];
        if( use_cache ) __cache_set_vid(vid);

        std::array<int, VARGC>  ai{ _Index_fetcher<_VIs>::get(__args, vid[_VIs])... };

        auto vfunc = multi_array_getAt(__get_vtable(), ai);

        return vfunc(static_cast<Self*>(__this), std::forward<_Args>(std::get< iseq_at<_AIs, typename VSig::index_to_normal> >(__args))...);
    }


private:
    static int &__cache_get_vid(int i)          { return __MFCache<>::get_vid(i); }
    static void __cache_set_vid(int *vid)       { __MFCache<>::set_vid(vid); }

public:

    //vtype--SI
    template<int I, typename T>
    static
    void _check1_SI(bool &error, T *arg, std::enable_if_t<is_vtype<T>::value> * =nullptr) {
        assert( arg );

        using TypeMap = vtypemap<typename std::tuple_element<I,domains>::type, typename std::tuple_element<I,BaseTypes>::type, __isMI>;
        TypeMap &tmap = TypeMap::get();

        int vid = arg->vtypeid();
        int mix = tmap._multimap[vid];
        if( mix == TypeMap::_MULTIMAP_CELL_UNDEFINED ) {
            if( tmap.update(vid, arg) == TypeMap::_MULTIMAP_CELL_INVALID ) {
                error = true;
                assert( tmap._typemap[vid] == TypeMap::_TYPEMAP_CELL_UNDEFINED );
            }
        }
        else if( mix == TypeMap::_MULTIMAP_CELL_INVALID ) {
            error = true;
            assert( tmap._typemap[vid] == TypeMap::_TYPEMAP_CELL_UNDEFINED );
        }
    }

    //poly--SI
    template<int I, typename T>
    static
    void _check1_SI(bool &error, const T *arg, std::enable_if_t<!is_vtype<T>::value>* =nullptr) {
        assert( arg );

        using TypeMap = vtypemap_poly<typename std::tuple_element<I,domains>::type, typename std::tuple_element<I,BaseTypes>::type, __isMI>;
        TypeMap &tmap = TypeMap::get();

        const auto vid = &typeid(*arg);
        if( tmap.update(vid, arg) == TypeMap::_TYPEMAP_CELL_INVALID)
            error = true;
    }

    static _Return_type
    _check_error(std::false_type, FX *fx, _NArgs...args)
    {
        std::tuple<_NArgs...>  __args{std::forward<_NArgs>(args)...};

        bool error = false;
        std::make_tuple((_check1_SI<_VIs>(error, get_base_ptr(std::get<_VIs>(__args))),0)...);

        return !error
            ? __vcall(static_cast<Self*>(fx), std::forward<_NArgs>(args)...)
            : __error_OOD(fx, std::forward<_Args>(std::get< iseq_at<_AIs, typename VSig::index_to_normal> >(__args))...);
    }
    //-----------------------------------------------------------------------------------------------------


    struct __Check1_MI
    {
        std::array<int,VARGC>  _ai;
        std::array<int,VARGC>  _vid;

        bool _match;
        bool _isMI;


        //poly
        template<int I, typename T>
        void prefetch_index(iseq<I>, T *arg, std::enable_if_t<!is_vtype<T>::value>* = nullptr) {
            using TypeMap = vtypemap_poly<typename std::tuple_element<I,domains>::type, typename std::tuple_element<I,BaseTypes>::type, __isMI>;
            TypeMap &tmap = TypeMap::get();
            
            _ai[I] = tmap._typemap[ std::get<I>(_vid) = __cache_get_vid(I) ];
        }

        //vtype
        template<int I, typename T>
        void prefetch_index(iseq<I>, T *arg, std::enable_if_t<is_vtype<T>::value>* = nullptr) {
            using TypeMap = vtypemap<typename std::tuple_element<I,domains>::type, typename std::tuple_element<I,BaseTypes>::type, __isMI>;
            TypeMap &tmap = TypeMap::get();
            int vid = std::get<I>(_vid) = arg->vtypeid();
            _ai[I] = tmap._typemap[vid] | tmap._multimap[vid];
        }

        //poly
        template<int I, typename T>
        void poly_adjust_ai(iseq<I>, T *arg, std::enable_if_t<!is_vtype<T>::value>* = nullptr) {
            using TypeMap = vtypemap_poly<typename std::tuple_element<I,domains>::type, typename std::tuple_element<I,BaseTypes>::type, __isMI>;
            _ai[I] &= TypeMap::_TYPEMAP_CELL_MASK_INDEX;
        }


        template<int I, typename T>
        void poly_adjust_ai(iseq<I>, T *arg, std::enable_if_t<is_vtype<T>::value>* = nullptr) { }

        //vtype--MI
        template<int I, typename T>
        void _check1_MI(iseq<I>, const T *arg, std::enable_if_t<is_vtype<T>::value> * =nullptr)
        {
            using TypeMap = vtypemap<typename std::tuple_element<I,domains>::type, typename std::tuple_element<I,BaseTypes>::type, __isMI>;
            TypeMap &tmap = TypeMap::get();

            int vid = arg->vtypeid();
            int mix = tmap._multimap[vid];

            mix = tmap.update(vid,arg);
            assert( mix != TypeMap::_MULTIMAP_CELL_UNDEFINED );

            if( mix == TypeMap::_MULTIMAP_CELL_INVALID ) {
                _match = false;
            }
            else if( mix==0 ) {
                assert( tmap._typemap[vid] );
                _ai[I] = tmap._typemap[vid];
            }
            else {
                _isMI   = true;
                _ai[I] = mix;
                assert( mix == tmap._multimap[vid] );
            }
        }

        //poly--MI
        template<int I, typename T>
        void _check1_MI(iseq<I>, const T *arg, std::enable_if_t<!is_vtype<T>::value> * =nullptr)
        {
            assert( arg );

            using TypeMap = vtypemap_poly<typename std::tuple_element<I,domains>::type, typename std::tuple_element<I,BaseTypes>::type, __isMI>;
            TypeMap &tmap = TypeMap::get();

            int vid = std::get<I>(_vid);
            if( vid==0 ) {
                vid = tmap.update(arg);
            }
            assert( vid );

            int tix;
            int tm = tmap._typemap[vid];
            _ai[I] = tm;

            if( tm == TypeMap::_TYPEMAP_CELL_INVALID )
                _match = false;
            else if( tix=tmap.extract_type_index(tm), !tix ) {
                _isMI   = true;
            }
        }
    };//end--struct __Check1_MI


    static _Return_type 
    _check_error(std::true_type, FX *fx, _NArgs...args)
    {
        std::tuple<_NArgs...>  __args{std::forward<_NArgs>(args)...};

        __Check1_MI  check;
        std::make_tuple((check.prefetch_index(iseq<_VIs>(), get_base_ptr(std::get<_VIs>(__args))),0)...);

        VFunc vfunc = __get_mvfunc_map()[ check._ai ];
        if( vfunc )
            return vfunc(fx, std::forward<_Args>(std::get< iseq_at<_AIs, typename VSig::index_to_normal> >(__args))...);


        std::array<int, VARGC> pBases;
        std::array<int, VARGC> nBases;


        check._isMI  = false;
        check._match = true;
        std::make_tuple((check._check1_MI(iseq<_VIs>(), get_base_ptr(std::get<_VIs>(__args))),0)...);

        if( ! check._match ) {
            vfunc = __error_OOD;
        }
        else if( ! check._isMI ) {
            std::make_tuple((check.poly_adjust_ai(iseq<_VIs>(), get_base_ptr(std::get<_VIs>(__args))),0)...);

            vfunc = multi_array_getAt(__get_vtable(), check._ai);
            assert( vfunc );
        }
        else {
                auto castmaps = __get_castmaps(check._vid);

                constexpr auto _CASTMAP_CELL_INVALID = _var_castmap_common<int>::_CASTMAP_CELL_INVALID;

                std::array<int,  VARGC> _nBases;
                std::array<int*, VARGC> _pBases;

                gstack<>  gs;
                auto buffer = gs.alloc<int[]>(linear_sizeof_tuples<domains>);
                int *gather = buffer.get();
                for(int i=0; i<VARGC ;++i) {
                    _pBases[i] = gather;

                    const auto &cmap = castmaps[i];
                    int extent = domains_sizes<domains>()[i];
                    for(int j=0;  j < extent  ;++j) {
                        if( cmap[j] != _CASTMAP_CELL_INVALID )
                            *gather++ = j;
                    }
                    _nBases[i] = gather - _pBases[i];
                }

                std::vector<std::array<int,VARGC>, gstack_allocator<std::array<int,VARGC>>>  sigs[2];
                foreach_cartesian<VARGC>(
                    _nBases,
                    [&_pBases,&sigs](std::array<int,VARGC> &key) {
                        std::array<int,VARGC>  index, index_func;
                        for(int i=0; i <VARGC ;++i) {
                            auto x = index[i] = _pBases[i][key[i]];
                            index_func[i] = x + 1;
                        }
                        auto func = multi_array_getAt(__get_vtable(), index_func);
                        if( multi_array_getAt(__get_sigtable(), index) ) {
                            assert( func );
                            if( func != __error_NO_MATCH && func )
                                sigs[0].push_back(index);
                        }
                        else if( func != __error_NO_MATCH ) {
                            sigs[1].push_back(index);
                        }
                    } 
                );


                for(auto &sigs : sigs )
                {
                    if( 1==sigs.size() ) {
                        for(auto &x : sigs[0] ) ++x;
                        vfunc = multi_array_getAt(__get_vtable(), sigs[0]);
                                                                                assert( vfunc != __error_NO_MATCH );
                        break;
                    }
                    else if( 1 < sigs.size() ) {
                        {
                            const std::type_info *arg_tidv[] = { &typeid(*const_cast<typename remove_pointer_or_reference<typename std::tuple_element<_VIs,virtual_arg_types>::type>::type*>(get_base_ptr(std::get<_VIs>(__args))))... };
                            auto sigc = reduce_sigs<domains,virtual_arg_types>(sigs, castmaps, _var_castmap_common<int>::_CASTMAP_CELL_INVALID, arg_tidv);
                            sigs.resize(sigc);
                        }

                        if ( 1 == sigs.size() ) {
                            for(auto &x : sigs[0] ) ++x;
                            vfunc = multi_array_getAt(__get_vtable(), sigs[0]);
                            break;
                        }
                    }
                }

                if( !vfunc )
                    vfunc = __error_NO_MATCH;
        }


        if( check._isMI ) {
            __get_mvfunc_map()[check._ai] = vfunc;
        }

        return vfunc(fx, std::forward<_Args>(std::get< iseq_at<_AIs, typename VSig::index_to_normal> >(__args))...);

    }//end-- _check_error


    static
    _Return_type _check_error(FX *fx, _Args...args)
    {
        std::tuple<_Args...>  __args{std::forward<_Args>(args)...};

        return _check_error(std::integral_constant<bool,__isMI>(), fx, std::forward<_NArgs>(std::get<iseq_at<_AIs, index_from_normal>>(__args))...);
    }


protected:

    static const vtable_type &__get_vtable() {
        return __vtable;
    }
    static const sigtable_type &__get_sigtable() {
        return __sigtable;
    }


    static const vtable_type    __vtable;
    static const sigtable_type  __sigtable;
    using __mvfunc_map_type = Map<std::array<int,VARGC>, VFunc, _Hash<std::array<int,VARGC>>>;
    static __mvfunc_map_type  __mvfunc_map;

    static auto &__get_mvfunc_map() {
        return __mvfunc_map;
    }
};




template<typename FX, bool MI, template <typename...> class Map, typename..._Args, size_t..._AIs, size_t..._VIs, typename..._NArgs>
const typename multi_func_TM<FX,MI,Map,std::tuple<_Args...>, std::index_sequence<_AIs...>, std::index_sequence<_VIs...>, std::tuple<_NArgs...>>::vtable_type
     multi_func_TM<FX,MI,Map,std::tuple<_Args...>, std::index_sequence<_AIs...>, std::index_sequence<_VIs...>, std::tuple<_NArgs...>>::__vtable
     = VTableBuilder::get_table();

template<typename FX, bool MI, template <typename...> class Map, typename..._Args, size_t..._AIs, size_t..._VIs, typename..._NArgs>
const typename multi_func_TM<FX,MI,Map,std::tuple<_Args...>, std::index_sequence<_AIs...>, std::index_sequence<_VIs...>, std::tuple<_NArgs...>>::sigtable_type
     multi_func_TM<FX,MI,Map,std::tuple<_Args...>, std::index_sequence<_AIs...>, std::index_sequence<_VIs...>, std::tuple<_NArgs...>>::__sigtable
     = SigTableBuilder::get_table();

template<typename FX, bool MI, template <typename...> class Map, typename..._Args, size_t..._AIs, size_t..._VIs, typename..._NArgs>
typename multi_func_TM<FX,MI,Map,std::tuple<_Args...>, std::index_sequence<_AIs...>, std::index_sequence<_VIs...>, std::tuple<_NArgs...>>::__mvfunc_map_type
    multi_func_TM<FX,MI,Map,std::tuple<_Args...>, std::index_sequence<_AIs...>, std::index_sequence<_VIs...>, std::tuple<_NArgs...>>::__mvfunc_map;





//////////////////////////////////////////////////////////////////////////////////////////////////
template<typename FX, bool MI,
        template <typename...> class Map = std::unordered_map,
        typename Args=typename resolve_virtual_signature<typename FX::type>::arg_types, 
        typename _return_type = typename resolve_virtual_signature<typename FX::type>::return_type
        >
struct multi_func_TM_i;



template<typename FX, bool MI, template <typename...> class Map, typename..._Args, typename _return_type>
struct multi_func_TM_i<FX, MI, Map, std::tuple<_Args...>, _return_type>
    : multi_func_TM<FX,MI,Map>
{
    using multi_func_TM<FX,MI,Map>::multi_func_TM;

    _return_type operator()(_Args...args) {
        return multi_func_TM<FX,MI,Map>::_call(std::forward<_Args>(args)...);
    }

    _return_type call(_Args...args) {
        return multi_func_TM<FX,MI,Map>::_call(std::forward<_Args>(args)...);
    }
};


template<typename FX, bool MI, template <typename...> class Map, typename..._Args>
struct multi_func_TM_i<FX, MI, Map, std::tuple<_Args...>, void>
    : multi_func_TM<FX,MI,Map>
{
    using multi_func_TM<FX,MI,Map>::multi_func_TM;

    void operator()(_Args...args) {
        multi_func_TM<FX,MI,Map>::_call(std::forward<_Args>(args)...);
    }

    void call(_Args...args) {
        multi_func_TM<FX,MI,Map>::_call(std::forward<_Args>(args)...);
    }
};



}//end-namespace vane_detail////////////////////////////////////////////////////////


template<typename...Ts>
using virtual_func = vane_detail::virtual_func_nonVf<Ts...>;

template<typename FX, bool MI=false, template <typename...> class Map=std::unordered_map, typename...Ts>
using multi_func  = vane_detail::multi_func_TM_i<FX,MI,Map,Ts...>;



}//namespace vane//////////////////////////////////////////////////////////////////////////////////////////////////////////////



namespace std {/////////////////////////////////////////////////////////////////
    template<size_t I, typename T, size_t SIZE>
    inline
    T &get(vane::gs_array<T,SIZE> &a) {
        static_assert(I < SIZE, "index is out of bounds");
        return a[I];
    }

    template<typename T, size_t SIZE>
    struct tuple_size<vane::gs_array<T,SIZE>>
    : public integral_constant<size_t, SIZE> { };


    template<size_t I, typename T, size_t N>
    struct tuple_element<I, vane::gs_array<T,N> > {
        using type = T;
    };

    template<typename __T, unsigned __GSI, typename __Traits>
    struct hash<vane::basic_gs_string<__T,__GSI,__Traits>> : __hash_base<size_t, vane::basic_gs_string<__T,__GSI,__Traits>>
    {
        size_t
        operator()(const vane::basic_gs_string<__T,__GSI,__Traits> &__s) const noexcept
        { return std::_Hash_impl::hash(__s.data(), __s.length()); }
    };

    template<typename __T, unsigned __GSI, typename __Traits>
    struct __is_fast_hash<hash<vane::basic_gs_string<__T,__GSI,__Traits>>> : std::false_type { };

}//end-namespace std////////////////////////////////////////////////////////////





// vane_duck_INTERFACE ////////////////////////////////////////////////
/*
    For macro meta-programming, I used the library:
        `Cloak' - "A mini-preprocessor library to demonstrate the recursive capabilities of the preprocessor"
    It's a small (128 lines) libray, and not appropriate to be used as it is, so I modified it.
    I appreciate his work.
    You can get it at:
        https://github.com/pfultz2/Cloak/blob/master/cloak.h
*/

#define VANE_PP_VA_ARG_0(_0,...) _0
#define VANE_PP_VA_ARG_1(_0,_1,...) _1
#define VANE_PP_VA_ARG_2(_0,_1,_2,...) _2
#define VANE_PP_VA_ARG_3(_0,_1,_2,_3,...) _3
#define VANE_PP_VA_ARG_4(_0,_1,_2,_3,_4,...) _4
#define VANE_PP_VA_ARG_5(_0,_1,_2,_3,_4,_5,...) _5
#define VANE_PP_VA_ARG_6(_0,_1,_2,_3,_4,_5,_6,...) _6
#define VANE_PP_VA_ARG_7(_0,_1,_2,_3,_4,_5,_6,_7,...) _7
#define VANE_PP_VA_ARG_8(_0,_1,_2,_3,_4,_5,_6,_7,_8,...) _8
#define VANE_PP_VA_ARG_9(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,...) _9
#define VANE_PP_VA_ARG_10(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,...) _10
#define VANE_PP_VA_ARG_11(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,...) _11
#define VANE_PP_VA_ARG_12(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,...) _12
#define VANE_PP_VA_ARG_13(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,...) _13
#define VANE_PP_VA_ARG_14(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,...) _14
#define VANE_PP_VA_ARG_15(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,...) _15
#define VANE_PP_VA_ARG_16(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,...) _16
#define VANE_PP_VA_ARG_17(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,_17,...) _17
#define VANE_PP_VA_ARG_18(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,_17,_18,...) _18
#define VANE_PP_VA_ARG_19(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,_17,_18,_19,...) _19
#define VANE_PP_VA_ARG_20(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,_17,_18,_19,_20,...) _20
#define VANE_PP_VA_ARG_21(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,_17,_18,_19,_20,_21,...) _21
#define VANE_PP_VA_ARG_22(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,_17,_18,_19,_20,_21,_22,...) _22
#define VANE_PP_VA_ARG_23(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,_17,_18,_19,_20,_21,_22,_23,...) _23
#define VANE_PP_VA_ARG_24(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,_17,_18,_19,_20,_21,_22,_23,_24,...) _24
#define VANE_PP_VA_ARG_25(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,_17,_18,_19,_20,_21,_22,_23,_24,_25,...) _25
#define VANE_PP_VA_ARG_26(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,_17,_18,_19,_20,_21,_22,_23,_24,_25,_26,...) _26
#define VANE_PP_VA_ARG_27(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,_17,_18,_19,_20,_21,_22,_23,_24,_25,_26,_27,...) _27
#define VANE_PP_VA_ARG_28(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,_17,_18,_19,_20,_21,_22,_23,_24,_25,_26,_27,_28,...) _28
#define VANE_PP_VA_ARG_29(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,_17,_18,_19,_20,_21,_22,_23,_24,_25,_26,_27,_28,_29,...) _29
#define VANE_PP_VA_ARG_30(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,_17,_18,_19,_20,_21,_22,_23,_24,_25,_26,_27,_28,_29,_30,...) _30
#define VANE_PP_VA_ARG_31(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,_17,_18,_19,_20,_21,_22,_23,_24,_25,_26,_27,_28,_29,_30,_31,...) _31
#define VANE_PP_VA_ARG_32(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,_17,_18,_19,_20,_21,_22,_23,_24,_25,_26,_27,_28,_29,_30,_31,_32,...) _32
#define VANE_PP_VA_ARG_33(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,_17,_18,_19,_20,_21,_22,_23,_24,_25,_26,_27,_28,_29,_30,_31,_32,_33,...) _33
#define VANE_PP_VA_ARG_34(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,_17,_18,_19,_20,_21,_22,_23,_24,_25,_26,_27,_28,_29,_30,_31,_32,_33,_34,...) _34
#define VANE_PP_VA_ARG_35(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,_17,_18,_19,_20,_21,_22,_23,_24,_25,_26,_27,_28,_29,_30,_31,_32,_33,_34,_35,...) _35
#define VANE_PP_VA_ARG_36(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,_17,_18,_19,_20,_21,_22,_23,_24,_25,_26,_27,_28,_29,_30,_31,_32,_33,_34,_35,_36,...) _36
#define VANE_PP_VA_ARG_37(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,_17,_18,_19,_20,_21,_22,_23,_24,_25,_26,_27,_28,_29,_30,_31,_32,_33,_34,_35,_36,_37,...) _37
#define VANE_PP_VA_ARG_38(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,_17,_18,_19,_20,_21,_22,_23,_24,_25,_26,_27,_28,_29,_30,_31,_32,_33,_34,_35,_36,_37,_38,...) _38
#define VANE_PP_VA_ARG_39(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,_17,_18,_19,_20,_21,_22,_23,_24,_25,_26,_27,_28,_29,_30,_31,_32,_33,_34,_35,_36,_37,_38,_39,...) _39
#define VANE_PP_VA_ARG_40(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,_17,_18,_19,_20,_21,_22,_23,_24,_25,_26,_27,_28,_29,_30,_31,_32,_33,_34,_35,_36,_37,_38,_39,_40,...) _40
#define VANE_PP_VA_ARG_41(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,_17,_18,_19,_20,_21,_22,_23,_24,_25,_26,_27,_28,_29,_30,_31,_32,_33,_34,_35,_36,_37,_38,_39,_40,_41,...) _41
#define VANE_PP_VA_ARG_42(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,_17,_18,_19,_20,_21,_22,_23,_24,_25,_26,_27,_28,_29,_30,_31,_32,_33,_34,_35,_36,_37,_38,_39,_40,_41,_42,...) _42
#define VANE_PP_VA_ARG_43(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,_17,_18,_19,_20,_21,_22,_23,_24,_25,_26,_27,_28,_29,_30,_31,_32,_33,_34,_35,_36,_37,_38,_39,_40,_41,_42,_43,...) _43
#define VANE_PP_VA_ARG_44(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,_17,_18,_19,_20,_21,_22,_23,_24,_25,_26,_27,_28,_29,_30,_31,_32,_33,_34,_35,_36,_37,_38,_39,_40,_41,_42,_43,_44,...) _44
#define VANE_PP_VA_ARG_45(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,_17,_18,_19,_20,_21,_22,_23,_24,_25,_26,_27,_28,_29,_30,_31,_32,_33,_34,_35,_36,_37,_38,_39,_40,_41,_42,_43,_44,_45,...) _45
#define VANE_PP_VA_ARG_46(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,_17,_18,_19,_20,_21,_22,_23,_24,_25,_26,_27,_28,_29,_30,_31,_32,_33,_34,_35,_36,_37,_38,_39,_40,_41,_42,_43,_44,_45,_46,...) _46
#define VANE_PP_VA_ARG_47(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,_17,_18,_19,_20,_21,_22,_23,_24,_25,_26,_27,_28,_29,_30,_31,_32,_33,_34,_35,_36,_37,_38,_39,_40,_41,_42,_43,_44,_45,_46,_47,...) _47
#define VANE_PP_VA_ARG_48(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,_17,_18,_19,_20,_21,_22,_23,_24,_25,_26,_27,_28,_29,_30,_31,_32,_33,_34,_35,_36,_37,_38,_39,_40,_41,_42,_43,_44,_45,_46,_47,_48,...) _48
#define VANE_PP_VA_ARG_49(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,_17,_18,_19,_20,_21,_22,_23,_24,_25,_26,_27,_28,_29,_30,_31,_32,_33,_34,_35,_36,_37,_38,_39,_40,_41,_42,_43,_44,_45,_46,_47,_48,_49,...) _49
#define VANE_PP_VA_ARG_50(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,_17,_18,_19,_20,_21,_22,_23,_24,_25,_26,_27,_28,_29,_30,_31,_32,_33,_34,_35,_36,_37,_38,_39,_40,_41,_42,_43,_44,_45,_46,_47,_48,_49,_50,...) _50
#define VANE_PP_VA_ARG_51(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,_17,_18,_19,_20,_21,_22,_23,_24,_25,_26,_27,_28,_29,_30,_31,_32,_33,_34,_35,_36,_37,_38,_39,_40,_41,_42,_43,_44,_45,_46,_47,_48,_49,_50,_51,...) _51
#define VANE_PP_VA_ARG_52(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,_17,_18,_19,_20,_21,_22,_23,_24,_25,_26,_27,_28,_29,_30,_31,_32,_33,_34,_35,_36,_37,_38,_39,_40,_41,_42,_43,_44,_45,_46,_47,_48,_49,_50,_51,_52,...) _52
#define VANE_PP_VA_ARG_53(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,_17,_18,_19,_20,_21,_22,_23,_24,_25,_26,_27,_28,_29,_30,_31,_32,_33,_34,_35,_36,_37,_38,_39,_40,_41,_42,_43,_44,_45,_46,_47,_48,_49,_50,_51,_52,_53,...) _53
#define VANE_PP_VA_ARG_54(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,_17,_18,_19,_20,_21,_22,_23,_24,_25,_26,_27,_28,_29,_30,_31,_32,_33,_34,_35,_36,_37,_38,_39,_40,_41,_42,_43,_44,_45,_46,_47,_48,_49,_50,_51,_52,_53,_54,...) _54
#define VANE_PP_VA_ARG_55(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,_17,_18,_19,_20,_21,_22,_23,_24,_25,_26,_27,_28,_29,_30,_31,_32,_33,_34,_35,_36,_37,_38,_39,_40,_41,_42,_43,_44,_45,_46,_47,_48,_49,_50,_51,_52,_53,_54,_55,...) _55
#define VANE_PP_VA_ARG_56(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,_17,_18,_19,_20,_21,_22,_23,_24,_25,_26,_27,_28,_29,_30,_31,_32,_33,_34,_35,_36,_37,_38,_39,_40,_41,_42,_43,_44,_45,_46,_47,_48,_49,_50,_51,_52,_53,_54,_55,_56,...) _56
#define VANE_PP_VA_ARG_57(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,_17,_18,_19,_20,_21,_22,_23,_24,_25,_26,_27,_28,_29,_30,_31,_32,_33,_34,_35,_36,_37,_38,_39,_40,_41,_42,_43,_44,_45,_46,_47,_48,_49,_50,_51,_52,_53,_54,_55,_56,_57,...) _57
#define VANE_PP_VA_ARG_58(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,_17,_18,_19,_20,_21,_22,_23,_24,_25,_26,_27,_28,_29,_30,_31,_32,_33,_34,_35,_36,_37,_38,_39,_40,_41,_42,_43,_44,_45,_46,_47,_48,_49,_50,_51,_52,_53,_54,_55,_56,_57,_58,...) _58
#define VANE_PP_VA_ARG_59(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,_17,_18,_19,_20,_21,_22,_23,_24,_25,_26,_27,_28,_29,_30,_31,_32,_33,_34,_35,_36,_37,_38,_39,_40,_41,_42,_43,_44,_45,_46,_47,_48,_49,_50,_51,_52,_53,_54,_55,_56,_57,_58,_59,...) _59
#define VANE_PP_VA_ARG_60(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,_17,_18,_19,_20,_21,_22,_23,_24,_25,_26,_27,_28,_29,_30,_31,_32,_33,_34,_35,_36,_37,_38,_39,_40,_41,_42,_43,_44,_45,_46,_47,_48,_49,_50,_51,_52,_53,_54,_55,_56,_57,_58,_59,_60,...) _60
#define VANE_PP_VA_ARG_61(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,_17,_18,_19,_20,_21,_22,_23,_24,_25,_26,_27,_28,_29,_30,_31,_32,_33,_34,_35,_36,_37,_38,_39,_40,_41,_42,_43,_44,_45,_46,_47,_48,_49,_50,_51,_52,_53,_54,_55,_56,_57,_58,_59,_60,_61,...) _61
#define VANE_PP_VA_ARG_62(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,_17,_18,_19,_20,_21,_22,_23,_24,_25,_26,_27,_28,_29,_30,_31,_32,_33,_34,_35,_36,_37,_38,_39,_40,_41,_42,_43,_44,_45,_46,_47,_48,_49,_50,_51,_52,_53,_54,_55,_56,_57,_58,_59,_60,_61,_62,...) _62
#define VANE_PP_VA_ARG_63(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,_17,_18,_19,_20,_21,_22,_23,_24,_25,_26,_27,_28,_29,_30,_31,_32,_33,_34,_35,_36,_37,_38,_39,_40,_41,_42,_43,_44,_45,_46,_47,_48,_49,_50,_51,_52,_53,_54,_55,_56,_57,_58,_59,_60,_61,_62,_63,...) _63
#define VANE_PP_VA_ARG_64(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,_17,_18,_19,_20,_21,_22,_23,_24,_25,_26,_27,_28,_29,_30,_31,_32,_33,_34,_35,_36,_37,_38,_39,_40,_41,_42,_43,_44,_45,_46,_47,_48,_49,_50,_51,_52,_53,_54,_55,_56,_57,_58,_59,_60,_61,_62,_63,_64,...) _64
#define VANE_PP_VA_ARG_MAX  VANE_PP_VA_ARG_64


#define VANE_PP_STRINGIZE(...)      VANE_PP_STRINGIZE_1(__VA_ARGS__)
#define VANE_PP_STRINGIZE_1(...)    VANE_PP_STRINGIZE_2(__VA_ARGS__)
#define VANE_PP_STRINGIZE_2(...)    #__VA_ARGS__


#define VANE_PP_EXPAND_(...)    __VA_ARGS__
#define VANE_PP_EXPAND_L1_(...) __VA_ARGS__
#define VANE_PP_EXPAND_L2_(...) __VA_ARGS__
#define VANE_PP_EXPAND_L3_(...) __VA_ARGS__
#define VANE_PP_EXPAND_L4_(...) __VA_ARGS__

#define VANE_PP_GLUE(x, y)      x y
#define VANE_PP_GLUE_(x, y)     x y
#define VANE_PP_GLUE_L2_(x, y)  x y
#define VANE_PP_GLUE_L3_(x, y)  x y
#define VANE_PP_GLUE_L4_(x, y)  x y

#define __VA_COUNT(...)         VANE_PP_GLUE_L4_(VANE_PP_VA_ARG_64,(dummy,##__VA_ARGS__,63,62,61,60,59,58,57,56,55,54,53,52,51,50,49,48,47,46,45,44,43,42,41,40,39,38,37,36,35,34,33,32,31,30,29,28,27,26,25,24,23,22,21,20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0))
#define VANE_PP_VA_COUNT(...)   __VA_COUNT(__VA_ARGS__)
#define VANE_PP_TUPLE_SIZE(tuple)   __VA_COUNT tuple

#define VANE_PP_TUPLE_ELEM(i,tuple) VANE_PP_VA_ARG_ ## i tuple
#define VANE_PP_VA_ARG(i, ...)      VANE_PP_TUPLE_ELEM(i, (__VA_ARGS__))

#define VANE_PP_MK_ARGS_10(x)   x,x,x,x,x,x,x,x,x,x
#define VANE_PP_MK_ARGS_20(x)   VANE_PP_MK_ARGS_10(x),VANE_PP_MK_ARGS_10(x)
#define VANE_PP_MK_ARGS_60(x)   VANE_PP_MK_ARGS_20(x),VANE_PP_MK_ARGS_20(x),VANE_PP_MK_ARGS_20(x)

#define VANE_PP_CHECK(...)      VANE_PP_CHECK_1(__VA_ARGS__, VANE_PP_MK_ARGS_60(0))
#define VANE_PP_CHECK_1(...)    VANE_PP_CHECK_2((__VA_ARGS__))
#define VANE_PP_CHECK_2(a)      VANE_PP_VA_ARG_60 a

#define VANE_PP_PROBE(...)      VANE_PP_PROBE_(__VA_ARGS__)     
#define VANE_PP_PROBE_(...)     VANE_PP_MK_ARGS_60(1),   __VA_ARGS__

#define VANE_PP_IS_EMPTY(...)   VANE_PP_IS_EMPTY_(__VA_ARGS__)
#define VANE_PP_IS_EMPTY_(...)  VANE_PP_CHECK(VANE_PP_PROBE_EMPTY __VA_ARGS__ (VANE_PP_PROBE)(1))
#define VANE_PP_PROBE_EMPTY(...)    __VA_ARGS__
#define VANE_PP_IS_NOT_EMPTY(...)   VANE_PP_NOT( VANE_PP_IS_EMPTY(__VA_ARGS__) )

#define VANE_PP_IS(x,...)       VANE_PP_IS_(x,__VA_ARGS__)
#define VANE_PP_IS_(x,...)      VANE_PP_CHECK(VANE_PP_PROBE_IS_ ## x ## _ ##__VA_ARGS__)

#define VANE_PP_IS_ONLY(x,...)      VANE_PP_IS_ONLY_(x,__VA_ARGS__)
#define VANE_PP_IS_ONLY_(x,...) \
    VANE_PP_IIF( VANE_PP_IS_PAREN(__VA_ARGS__) ) ( \
        0,\
        VANE_PP_IS_ONLY__1((x,__VA_ARGS__)) \
    )
#define VANE_PP_IS_ONLY__1(tuple)   VANE_PP_IS_ONLY__2 tuple
#define VANE_PP_IS_ONLY__2(x,...)   VANE_PP_IS_ONLY__3(VANE_PP_PROBE_IS_ONLY_ ## x ## _ ##__VA_ARGS__ (VANE_PP_PROBE)(1))
#define VANE_PP_IS_ONLY__3(...)     VANE_PP_CHECK(__VA_ARGS__)

#define VANE_PP_PROBE_IS_ONLY_void_void(...)        __VA_ARGS__

#define VANE_PP_IS_QUOTE(...)   VANE_PP_IS(VANE_PP_QUOTE,__VA_ARGS__)
#define VANE_PP_PROBE_IS_VANE_PP_QUOTE_VANE_PP_QUOTE    VANE_PP_PROBE(1),
#define o_vane_quote_                           VANE_PP_QUOTE
#define o_vane_quote__                          VANE_PP_QUOTE
#define o_vane_quote___                         VANE_PP_QUOTE
#define o_vane_quote____                        VANE_PP_QUOTE
#define o_vane_quote_____                       VANE_PP_QUOTE
#define o_vane_quote______                      VANE_PP_QUOTE
#define o_vane_quote_______                     VANE_PP_QUOTE
#define o_vane_quote________                    VANE_PP_QUOTE
#define o_vane_quote_________                   VANE_PP_QUOTE
#define o_vane_quote__________                  VANE_PP_QUOTE
#define o_vane_quote___________                 VANE_PP_QUOTE
#define o_vane_quote____________                VANE_PP_QUOTE
#define o_vane_quote_____________               VANE_PP_QUOTE
#define o_vane_quote______________              VANE_PP_QUOTE
#define o_vane_quote_______________             VANE_PP_QUOTE
#define o_vane_quote________________            VANE_PP_QUOTE
#define o_vane_quote_________________           VANE_PP_QUOTE
#define o_vane_quote__________________          VANE_PP_QUOTE
#define o_vane_quote___________________         VANE_PP_QUOTE
#define o_vane_quote____________________        VANE_PP_QUOTE
#define o_vane_quote_____________________       VANE_PP_QUOTE
#define o_vane_quote______________________      VANE_PP_QUOTE
#define o_vane_quote_______________________     VANE_PP_QUOTE
#define o_vane_quote________________________    VANE_PP_QUOTE
#define o__vane_quote__                         VANE_PP_QUOTE
#define VANE_PP_EXPAND_VANE_PP_QUOTE(...)   __VA_ARGS__
#define VANE_PP_EXPAND_namespace(...)       __VA_ARGS__
#define VANE_PP_EXPAND_using(...)           __VA_ARGS__

#define VANE_PP_IS_NAMESPACE(...)   VANE_PP_IS(namespace,__VA_ARGS__)
#define VANE_PP_PROBE_IS_namespace_namespace    VANE_PP_PROBE(1),

#define VANE_PP_IS_USING(...)       VANE_PP_IS(using,__VA_ARGS__)
#define VANE_PP_PROBE_IS_using_using    VANE_PP_PROBE(1),

#define VANE_PP_CAT(a, ...)         VANE_PP_CAT_1__(a, __VA_ARGS__)
#define VANE_PP_CAT_1__(a, ...)     VANE_PP_CAT_2__(a, __VA_ARGS__)
#define VANE_PP_CAT_2__(a, ...)     a ## __VA_ARGS__

#define VANE_PP_CAT_(a, ...)        VANE_PP_CAT__1__(a, __VA_ARGS__)
#define VANE_PP_CAT__1__(a, ...)    VANE_PP_CAT__2__(a, __VA_ARGS__)
#define VANE_PP_CAT__2__(a, ...)    a ## __VA_ARGS__

#define VANE_PP_BITAND(x)   VANE_PP_CAT_(VANE_PP_BITAND_, x)
#define VANE_PP_BITAND_0(y) 0
#define VANE_PP_BITAND_1(y) y

#define VANE_PP_BITOR(x)    VANE_PP_CAT_(VANE_PP_BITOR_, x)
#define VANE_PP_BITOR_0(y)  y
#define VANE_PP_BITOR_1(y)  1

#define VANE_PP_INC(x)  VANE_PP_CAT_(VANE_PP_INC_, x)
#define VANE_PP_INC_0 1
#define VANE_PP_INC_1 2
#define VANE_PP_INC_2 3
#define VANE_PP_INC_3 4
#define VANE_PP_INC_4 5
#define VANE_PP_INC_5 6
#define VANE_PP_INC_6 7
#define VANE_PP_INC_7 8
#define VANE_PP_INC_8 9
#define VANE_PP_INC_9 10
#define VANE_PP_INC_10 11
#define VANE_PP_INC_11 12
#define VANE_PP_INC_12 13
#define VANE_PP_INC_13 14
#define VANE_PP_INC_14 15
#define VANE_PP_INC_15 16
#define VANE_PP_INC_16 17
#define VANE_PP_INC_17 18
#define VANE_PP_INC_18 19
#define VANE_PP_INC_19 20
#define VANE_PP_INC_20 21
#define VANE_PP_INC_21 22
#define VANE_PP_INC_22 23
#define VANE_PP_INC_23 24
#define VANE_PP_INC_24 25
#define VANE_PP_INC_25 26
#define VANE_PP_INC_26 27
#define VANE_PP_INC_27 28
#define VANE_PP_INC_28 29
#define VANE_PP_INC_29 30
#define VANE_PP_INC_30 31
#define VANE_PP_INC_31 32
#define VANE_PP_INC_32 33
#define VANE_PP_INC_33 34
#define VANE_PP_INC_34 35
#define VANE_PP_INC_35 36
#define VANE_PP_INC_36 37
#define VANE_PP_INC_37 38
#define VANE_PP_INC_38 39
#define VANE_PP_INC_39 40
#define VANE_PP_INC_40 41
#define VANE_PP_INC_41 42
#define VANE_PP_INC_42 43
#define VANE_PP_INC_43 44
#define VANE_PP_INC_44 45
#define VANE_PP_INC_45 46
#define VANE_PP_INC_46 47
#define VANE_PP_INC_47 48
#define VANE_PP_INC_48 49
#define VANE_PP_INC_49 50
#define VANE_PP_INC_50 51
#define VANE_PP_INC_51 52
#define VANE_PP_INC_52 53
#define VANE_PP_INC_53 54
#define VANE_PP_INC_54 55
#define VANE_PP_INC_55 56
#define VANE_PP_INC_56 57
#define VANE_PP_INC_57 58
#define VANE_PP_INC_58 59
#define VANE_PP_INC_59 60
#define VANE_PP_INC_60 61
#define VANE_PP_INC_61 62
#define VANE_PP_INC_62 63
#define VANE_PP_INC_63 64
#define VANE_PP_INC_64 65

#define VANE_PP_DEC(x)  VANE_PP_CAT_(VANE_PP_DEC_, x)
#define VANE_PP_DEC_1 0
#define VANE_PP_DEC_2 1
#define VANE_PP_DEC_3 2
#define VANE_PP_DEC_4 3
#define VANE_PP_DEC_5 4
#define VANE_PP_DEC_6 5
#define VANE_PP_DEC_7 6
#define VANE_PP_DEC_8 7
#define VANE_PP_DEC_9 8
#define VANE_PP_DEC_10 9
#define VANE_PP_DEC_11 10
#define VANE_PP_DEC_12 11
#define VANE_PP_DEC_13 12
#define VANE_PP_DEC_14 13
#define VANE_PP_DEC_15 14
#define VANE_PP_DEC_16 15
#define VANE_PP_DEC_17 16
#define VANE_PP_DEC_18 17
#define VANE_PP_DEC_19 18
#define VANE_PP_DEC_20 19
#define VANE_PP_DEC_21 20
#define VANE_PP_DEC_22 21
#define VANE_PP_DEC_23 22
#define VANE_PP_DEC_24 23
#define VANE_PP_DEC_25 24
#define VANE_PP_DEC_26 25
#define VANE_PP_DEC_27 26
#define VANE_PP_DEC_28 27
#define VANE_PP_DEC_29 28
#define VANE_PP_DEC_30 29
#define VANE_PP_DEC_31 30
#define VANE_PP_DEC_32 31
#define VANE_PP_DEC_33 32
#define VANE_PP_DEC_34 33
#define VANE_PP_DEC_35 34
#define VANE_PP_DEC_36 35
#define VANE_PP_DEC_37 36
#define VANE_PP_DEC_38 37
#define VANE_PP_DEC_39 38
#define VANE_PP_DEC_40 39
#define VANE_PP_DEC_41 40
#define VANE_PP_DEC_42 41
#define VANE_PP_DEC_43 42
#define VANE_PP_DEC_44 43
#define VANE_PP_DEC_45 44
#define VANE_PP_DEC_46 45
#define VANE_PP_DEC_47 46
#define VANE_PP_DEC_48 47
#define VANE_PP_DEC_49 48
#define VANE_PP_DEC_50 49
#define VANE_PP_DEC_51 50
#define VANE_PP_DEC_52 51
#define VANE_PP_DEC_53 52
#define VANE_PP_DEC_54 53
#define VANE_PP_DEC_55 54
#define VANE_PP_DEC_56 55
#define VANE_PP_DEC_57 56
#define VANE_PP_DEC_58 57
#define VANE_PP_DEC_59 58
#define VANE_PP_DEC_60 59
#define VANE_PP_DEC_61 60
#define VANE_PP_DEC_62 61
#define VANE_PP_DEC_63 62
#define VANE_PP_DEC_64 63


#define VANE_PP_IS_PAREN(...)       VANE_PP_CHECK(VANE_PP_IS_PAREN_PROBE __VA_ARGS__)
#define VANE_PP_IS_PAREN_PROBE(...) VANE_PP_PROBE(1),

#define VANE_PP_NOT(x)  VANE_PP_CHECK(VANE_PP_CAT_(VANE_PP_NOT_, x))
#define VANE_PP_NOT_0   VANE_PP_PROBE(1),

#define VANE_PP_COMPL(b)    VANE_PP_CAT_(VANE_PP_COMPL_, b)
#define VANE_PP_COMPL_0 1
#define VANE_PP_COMPL_1 0

#define VANE_PP_BOOL(x) VANE_PP_COMPL(VANE_PP_NOT(x))

#define VANE_PP_IIF(c)          VANE_PP_IIF_(VANE_PP_CAT_(VANE_PP_IIF_, c)) 
#define VANE_PP_IIF_(...)       __VA_ARGS__ 
#define VANE_PP_IIF_0(t, ...)   __VA_ARGS__
#define VANE_PP_IIF_1(t, ...)   t

#define VANE_PP_IF(c)   VANE_PP_IIF(VANE_PP_BOOL(c))

#define VANE_PP_EAT(...)
#define VANE_PP_EXPAND(...)     __VA_ARGS__
#define VANE_PP_EXPAND_(...)    __VA_ARGS__

#define VANE_PP_WHEN(c)         VANE_PP_WHEN_(c)
#define VANE_PP_WHEN_(c)        VANE_PP_IF(c)(VANE_PP_EXPAND_, VANE_PP_EAT)
#define VANE_PP_WHEN_NOT(c)     VANE_PP_WHEN_NOT_(c)
#define VANE_PP_WHEN_NOT_(c)    VANE_PP_IF(c)(VANE_PP_EAT,VANE_PP_EXPAND_)

#define VANE_PP_EMPTY()
#define VANE_PP_DEFER(id)       id VANE_PP_EMPTY()
#define VANE_PP_OBSTRUCT(id)    id VANE_PP_DEFER(VANE_PP_EMPTY)()

#define VANE_PP_EVAL(...)  VANE_PP_EVAL1(VANE_PP_EVAL1(VANE_PP_EVAL1(__VA_ARGS__)))
#define VANE_PP_EVAL1(...) VANE_PP_EVAL2(VANE_PP_EVAL2(VANE_PP_EVAL2(__VA_ARGS__)))
#define VANE_PP_EVAL2(...) VANE_PP_EVAL3(VANE_PP_EVAL3(VANE_PP_EVAL3(__VA_ARGS__)))
#define VANE_PP_EVAL3(...) VANE_PP_EVAL4(VANE_PP_EVAL4(VANE_PP_EVAL4(__VA_ARGS__)))
#define VANE_PP_EVAL4(...) VANE_PP_EVAL5(VANE_PP_EVAL5(VANE_PP_EVAL5(__VA_ARGS__)))
#define VANE_PP_EVAL5(...) __VA_ARGS__

#define VANE_PP_REPEAT(count, macro, ...) \
    VANE_PP_REPEAT_(count, macro, __VA_ARGS__)

#define VANE_PP_REPEAT_(count, macro, ...) \
    VANE_PP_WHEN(count) \
    ( \
        VANE_PP_OBSTRUCT(VANE_PP_REPEAT_INDIRECT) () \
        ( \
            VANE_PP_DEC(count), macro, __VA_ARGS__ \
        ) \
        VANE_PP_OBSTRUCT(macro) \
        ( \
            VANE_PP_DEC(count), __VA_ARGS__ \
        ) \
    )
#define VANE_PP_REPEAT_INDIRECT()   VANE_PP_REPEAT


#define VANE_PP_WHILE(pred, op, ...) \
    VANE_PP_IF(VANE_PP_GLUE2_(pred,(__VA_ARGS__))) \
    ( \
        VANE_PP_OBSTRUCT(VANE_WHILE_INDIRECT) () \
        ( \
            pred, op, VANE_PP_GLUE2_(op,(__VA_ARGS__)) \
        ), \
        __VA_ARGS__ \
    )
#define VANE_WHILE_INDIRECT()   VANE_PP_WHILE

#define VANE_PP_COMMA(...) ,
#define VANE_PP_COMMA_IF(n) VANE_PP_IF(n)(VANE_PP_COMMA, VANE_PP_EAT)()

#define VANE_PP_SEMICOLON(...)  ;
#define VANE_PP_SEMICOLON_IF(n) VANE_PP_IF(n)(VANE_PP_SEMICOLON, VANE_PP_EAT)()

#define VANE_PP_VA_COUNT_EC(...)        VANE_PP_IF(VANE_PP_IS_EMPTY_PL(__VA_ARGS__))(0,VANE_PP_VA_COUNT(__VA_ARGS__))
#define VANE_PP_TUPLE_SIZE_EC(tuple)    VANE_PP_VA_COUNT_EC tuple

#define VANE_PP_IS_EMPTY_PL(...)    VANE_PP_IS_EMPTY_PL_(__VA_ARGS__)
#define VANE_PP_IS_EMPTY_PL_(...)   VANE_PP_IS_EMPTY(__VA_ARGS__)

#define VANE_PP_COLON(...)  :
#define VANE_PP_COLON_IF(n) VANE_PP_IF(n)(VANE_PP_COLON, VANE_PP_EAT)()

#define VANE_UNSEQ(seq) VANE_PP_CAT(VANE_UNSEQ_,VANE_SEQ_SIZE(seq)) seq
#define VANE_UNSEQ_1(...) __VA_ARGS__ 
#define VANE_UNSEQ_2(...) __VA_ARGS__ VANE_UNSEQ_1
#define VANE_UNSEQ_3(...) __VA_ARGS__ VANE_UNSEQ_2
#define VANE_UNSEQ_4(...) __VA_ARGS__ VANE_UNSEQ_3
#define VANE_UNSEQ_5(...) __VA_ARGS__ VANE_UNSEQ_4
#define VANE_UNSEQ_6(...) __VA_ARGS__ VANE_UNSEQ_5

#define VANE_SEQ_SIZE(seq) VANE_PP_CAT(VANE_SEQ_SIZE_, VANE_SEQ_SIZE_0 seq)
#define VANE_SEQ_SIZE_0(...) VANE_SEQ_SIZE_1
#define VANE_SEQ_SIZE_1(...) VANE_SEQ_SIZE_2
#define VANE_SEQ_SIZE_2(...) VANE_SEQ_SIZE_3
#define VANE_SEQ_SIZE_3(...) VANE_SEQ_SIZE_4
#define VANE_SEQ_SIZE_4(...) VANE_SEQ_SIZE_5
#define VANE_SEQ_SIZE_5(...) VANE_SEQ_SIZE_6


#define VANE_SEQ_SIZE_VANE_SEQ_SIZE_0 0
#define VANE_SEQ_SIZE_VANE_SEQ_SIZE_1 1
#define VANE_SEQ_SIZE_VANE_SEQ_SIZE_2 2
#define VANE_SEQ_SIZE_VANE_SEQ_SIZE_3 3
#define VANE_SEQ_SIZE_VANE_SEQ_SIZE_4 4
#define VANE_SEQ_SIZE_VANE_SEQ_SIZE_5 5
#define VANE_SEQ_SIZE_VANE_SEQ_SIZE_6 6


#define VANE_PP_SEQ_POP_2(...)  (__VA_ARGS__),  VANE_PP_SEQ_POP_1
#define VANE_PP_SEQ_POP_1(...)  (__VA_ARGS__),


#include <array>
#include <functional>
#include <exception>

namespace vane {
    template<typename> struct  duck;

    struct bad_duck { };
    struct bad_duck_call : std::runtime_error {
        bad_duck_call(const char *fname) : std::runtime_error(std::string("bad_duck_call: ") + fname + "() is called") {}

        [[noreturn]] 
        static __vane_noinline
        void raise(const char *fname) {
            throw bad_duck_call(fname);
        }
    };

    template<int N>
    struct reducer {
        template<typename Op, typename T, typename It>
        static constexpr
        T get(Op op, T i, It p) {
            return op(*p, reducer<N-1>::get(i, p+1, op));
        }
    };
    template<>
    struct reducer<1> {
        template<typename Op, typename T, typename It>
        static constexpr
        T get(Op op, T i, It p) {
            return i;
        }
    };
}

#define vane_duck_INTERFACE(seq)    VANE_PP_DUCKINTERFACE(seq)
#define VANE_PP_DUCKINTERFACE(a)    VANE_PP_DUCKINTERFACE_1((VANE_PP_SEQ_POP_2 a))
#define VANE_PP_DUCKINTERFACE_1(a)  VANE_PP_DUCKINTERFACE_2 a

#define VANE_PP_DUCKINTERFACE_2(a,b,...) \
        VANE_PP_IF( VANE_PP_BITAND(VANE_PP_NOT(VANE_PP_IS_PAREN a))(VANE_PP_IS_USING a)) \
        (\
            VANE_PP_DUCKINTERFACE_USING,\
            VANE_PP_DUCKINTERFACE_DEF\
        )(a,b, __VA_ARGS__)

#define VANE_PP_DUCKINTERFACE_USING(upart,flist,...) \
    VANE_PP_WHEN_NOT( VANE_PP_IS_EMPTY(__VA_ARGS__) ) ( ;static_assert(0,"syntax error //@vane_duck_INTERFACE"); ) \
    VANE_PP_EVAL(VANE_PP_REPEAT(VANE_PP_VA_COUNT flist, VANE_PP_DUCKINTERFACE_USING_CHECKLIST, flist)) \
    VANE_PP_DUCKINTERFACE_DEF_DUCK(VANE_PP_EMPTY(),(VANE_PP_CAT_(VANE_PP_EXPAND_,VANE_PP_EXPAND upart)), flist)

#define VANE_PP_DUCKINTERFACE_USING_CHECKLIST(i, flist)     VANE_PP_DUCKINTERFACE_USING_CHECKLIST_I(i, VANE_PP_TUPLE_ELEM(i,flist))
#define VANE_PP_DUCKINTERFACE_USING_CHECKLIST_I(i, decl)    VANE_PP_WHEN_NOT( VANE_PP_IS_PAREN(decl) ) ( ;static_assert(0,"syntax error //@vane_duck_INTERFACE"); )

#define VANE_PP_DUCKINTERFACE_DEF(a,b,...) \
    VANE_PP_DUCKINTERFACE_DEF_1(\
        VANE_PP_IF( VANE_PP_BITAND(VANE_PP_NOT(VANE_PP_IS_PAREN a))(VANE_PP_IS_NAMESPACE a)) \
        ( \
            (a,b,__VA_ARGS__),\
            VANE_PP_IF( VANE_PP_BITAND(VANE_PP_NOT(VANE_PP_IS_PAREN b))(VANE_PP_IS_NAMESPACE b)) \
            (\
                (b,a, __VA_ARGS__),\
                (,a,b __VA_ARGS__)\
            )\
        )\
    )
#define VANE_PP_DUCKINTERFACE_DEF_1(a)  VANE_PP_DUCKINTERFACE_DEF_PARTS a
#define VANE_PP_DUCKINTERFACE_DEF_PARTS(nspart,namepart,flist,...) \
        VANE_PP_DUCKINTERFACE_DEF_IF(nspart,namepart, flist) \
        VANE_PP_DUCKINTERFACE_DEF_DUCK( nspart,namepart, flist)

#define VANE_PP_DUCKINTERFACE_DECLITEM_APPLY(m, a, seq) VANE_PP_DUCKINTERFACE_DECLITEM_APPLY_1(m, a, seq())
#define VANE_PP_DUCKINTERFACE_DECLITEM_APPLY_1(m, a, seq)   VANE_PP_DUCKINTERFACE_DECLITEM_APPLY_2(m, a, VANE_PP_SEQ_POP_1 seq)
#define VANE_PP_DUCKINTERFACE_DECLITEM_APPLY_2(...)         VANE_PP_GLUE_L2_(VANE_PP_DUCKINTERFACE_DECLITEM_APPLY_4,(__VA_ARGS__))
#define VANE_PP_DUCKINTERFACE_DECLITEM_APPLY_4(m,a0,a1,...) \
    VANE_PP_DUCKINTERFACE_DECLITEM_APPLY_5\
    VANE_PP_IF( VANE_PP_IS_PAREN( VANE_PP_EXPAND a1 ) ) (\
        ( m, a0, ([[VANE_PP_EXPAND_ VANE_PP_EXPAND a1]]), VANE_PP_SEQ_POP_1 __VA_ARGS__ ),\
        ( m, a0,(),a1, __VA_ARGS__)\
    )
#define VANE_PP_DUCKINTERFACE_DECLITEM_APPLY_5(...)             VANE_PP_GLUE_L3_(VANE_PP_DUCKINTERFACE_DECLITEM_APPLY_6,(__VA_ARGS__))
#define VANE_PP_DUCKINTERFACE_DECLITEM_APPLY_6(m,a0,a1,a2,...)  m(a0,a1,a2, VANE_UNSEQ(__VA_ARGS__))

#define VANE_PP_DUCKINTERFACE_DEF_IF(nspart, namepart, bodypart) \
    VANE_PP_GLUE_(VANE_PP_DUCKINTERFACE_DECL_IF,(nspart, VANE_PP_EXPAND namepart))\
    VANE_PP_DUCKINTERFACE_DEF_IF_BODY(bodypart) \
    VANE_PP_WHEN( VANE_PP_IS_NOT_EMPTY(nspart) ) ( } )

#define VANE_PP_DUCKINTERFACE_DECL_IF(__NS, __IF,...)\
    VANE_PP_DUCKINTERFACE_DECL_IF_1(\
            (VANE_PP_WHEN( VANE_PP_IS_NOT_EMPTY(__NS) ) ( VANE_PP_CAT_(VANE_PP_EXPAND_,VANE_PP_EXPAND __NS))) \
            , __IF,__VA_ARGS__)

#define VANE_PP_DUCKINTERFACE_DECL_IF_1(__NS, __IF,...)\
    VANE_PP_WHEN( VANE_PP_IS_NOT_EMPTY __NS ) ( \
        namespace VANE_PP_EXPAND __NS { \
    )\
    struct __vane_novtable __IF  __VA_ARGS__ {

#define VANE_PP_DUCKINTERFACE_DEF_IF_BODY(flist)\
        VANE_PP_EVAL(VANE_PP_REPEAT(VANE_PP_VA_COUNT flist, VANE_PP_DUCKINTERFACE_IF_DECL_ITEM, flist))\
    };

#define VANE_PP_DUCKINTERFACE_IF_DECL_ITEM(i, flist) \
    VANE_PP_DUCKINTERFACE_IF_DECL_ITEM_I(i, VANE_PP_TUPLE_ELEM(i,flist))

#define VANE_PP_DUCKINTERFACE_IF_DECL_ITEM_I(i, decl) \
    VANE_PP_IF(VANE_PP_IS_PAREN(decl))(\
        VANE_PP_DUCKINTERFACE_IF_DECL_FUNC1,\
        VANE_PP_EAT\
    )(i,decl)\
    VANE_PP_IF(VANE_PP_IS_PAREN(decl))(\
        VANE_PP_EMPTY(),\
        VANE_PP_IF( VANE_PP_IS_QUOTE(decl) ) (\
            VANE_PP_CAT(VANE_PP_EXPAND_,decl), \
            decl;\
        )\
    )

#define VANE_PP_DUCKINTERFACE_IF_DECL_FUNC1(i, seq) \
    VANE_PP_DUCKINTERFACE_DECLITEM_APPLY(VANE_PP_DUCKINTERFACE_IF_DECL_FUNC1_DO, dummy, seq)

#define VANE_PP_DUCKINTERFACE_IF_DECL_FUNC1_DO(dummy,attr,decl,...) \
    VANE_PP_EXPAND attr\
    virtual VANE_PP_TUPLE_ELEM(1,decl) \
    VANE_PP_TUPLE_ELEM(0,decl)(VANE_PP_REPEAT(VANE_PP_TUPLE_SIZE(VANE_PP_TUPLE_ELEM(2,decl)), VANE_PP_DUCKINTERFACE_DECL_IF_ARG1, VANE_PP_TUPLE_ELEM(2,decl)) ) __VA_ARGS__ = 0;

#define VANE_PP_DUCKINTERFACE_DECL_IF_ARG1(i, tags) \
    VANE_PP_COMMA_IF(i) VANE_PP_TUPLE_ELEM(i,tags)

#define VANE_PP_DUCKINTERFACE_BASE_DUCK_DECL_ITEM(i, flist) \
    VANE_PP_DUCKINTERFACE_BASE_DUCK_DECL_ITEM_I(i, VANE_PP_TUPLE_ELEM(i,flist))

#define VANE_PP_DUCKINTERFACE_BASE_DUCK_DECL_ITEM_I(i, decl) \
    VANE_PP_IF( VANE_PP_IS_PAREN(decl) ) ( VANE_PP_DUCKINTERFACE_BASE_DUCK_DECL_ITEM_FUNC1,VANE_PP_EAT)(i,decl)

#define VANE_PP_DUCKINTERFACE_BASE_DUCK_DECL_ITEM_FUNC1(i,seq)\
    VANE_PP_DUCKINTERFACE_DECLITEM_APPLY(VANE_PP_DUCKINTERFACE_BASE_DUCK_DECL_ITEM_FUNC1_DO, dummy, seq)

#define VANE_PP_DUCKINTERFACE_BASE_DUCK_DECL_ITEM_FUNC1_DO(dummy,attr,decl,...) \
    VANE_PP_EXPAND attr\
    VANE_PP_TUPLE_ELEM(1,decl) \
    VANE_PP_TUPLE_ELEM(0,decl) \
    (VANE_PP_REPEAT(VANE_PP_TUPLE_SIZE_EC(VANE_PP_TUPLE_ELEM(2,decl)), VANE_PP_DUCKINTERFACE_DECL_BASE_ARG1, VANE_PP_TUPLE_ELEM(2,decl)))\
    const { \
        return _get_obj()->VANE_PP_TUPLE_ELEM(0,decl)   \
        (VANE_PP_REPEAT(VANE_PP_TUPLE_SIZE_EC(VANE_PP_TUPLE_ELEM(2,decl)), VANE_PP_DUCKINTERFACE_DECL_CALL_ARG1, VANE_PP_TUPLE_ELEM(2,decl)) );\
    }\
    VANE_PP_EXPAND attr\
    VANE_PP_TUPLE_ELEM(1,decl) \
    VANE_PP_TUPLE_ELEM(0,decl) \
    (VANE_PP_REPEAT(VANE_PP_TUPLE_SIZE_EC(VANE_PP_TUPLE_ELEM(2,decl)), VANE_PP_DUCKINTERFACE_DECL_BASE_ARG1, VANE_PP_TUPLE_ELEM(2,decl)))\
    const volatile { \
        return _get_obj()->VANE_PP_TUPLE_ELEM(0,decl)   \
        (VANE_PP_REPEAT(VANE_PP_TUPLE_SIZE_EC(VANE_PP_TUPLE_ELEM(2,decl)), VANE_PP_DUCKINTERFACE_DECL_CALL_ARG1, VANE_PP_TUPLE_ELEM(2,decl)) );\
    }

#define VANE_PP_DUCKINTERFACE_DECL_BASE_ARG1(i, params) \
    VANE_PP_COMMA_IF(i) \
    VANE_PP_TUPLE_ELEM(i,params) \
    VANE_PP_IF( VANE_PP_IS_ONLY(void, VANE_PP_TUPLE_ELEM(i,params))) ( VANE_PP_EMPTY(), _ ## i )

#define VANE_PP_DUCKINTERFACE_DECL_CALL_ARG1(i, params) \
    VANE_PP_COMMA_IF(i) \
    VANE_PP_IF( VANE_PP_IS_ONLY(void, VANE_PP_TUPLE_ELEM(i,params))) ( VANE_PP_EMPTY(), _ ## i )

#define VANE_PP_DUCKINTERFACE_BAD_DUCK_DECL_ITEM(i, flist, ifname) \
    VANE_PP_DUCKINTERFACE_BAD_DUCK_DECL_ITEM_I(i, VANE_PP_TUPLE_ELEM(i,flist), ifname) \

#define VANE_PP_DUCKINTERFACE_BAD_DUCK_DECL_ITEM_I(i, decl, ifname) \
    VANE_PP_IF( VANE_PP_IS_PAREN(decl) ) ( VANE_PP_DUCKINTERFACE_BAD_DUCK_DECL_ITEM_FUNC1,VANE_PP_EAT)(i, decl, ifname)

#define VANE_PP_DUCKINTERFACE_BAD_DUCK_DECL_ITEM_FUNC1(i, seq, ifname) \
    VANE_PP_DUCKINTERFACE_DECLITEM_APPLY(VANE_PP_DUCKINTERFACE_BAD_DUCK_DECL_ITEM_FUNC1_DO, ifname, seq)

#define VANE_PP_DUCKINTERFACE_BAD_DUCK_DECL_ITEM_FUNC1_DO(ifname,attr, decl,...) \
    [[noreturn]]\
    static\
    VANE_PP_TUPLE_ELEM(1,decl) \
    VANE_PP_TUPLE_ELEM(0,decl) \
    (VANE_PP_REPEAT(VANE_PP_TUPLE_SIZE_EC(VANE_PP_TUPLE_ELEM(2,decl)), VANE_PP_DUCKINTERFACE_DECL_IF_ARG1, VANE_PP_TUPLE_ELEM(2,decl)) )\
    { DOUT_SIG() bad_duck_call::raise(VANE_PP_STRINGIZE( ifname::VANE_PP_TUPLE_ELEM(0,decl) )); }

#define VANE_PP_DUCKINTERFACE_T_DUCK_DECL_ITEM(i, flist)    VANE_PP_DUCKINTERFACE_T_DUCK_DECL_ITEM_I(i, VANE_PP_TUPLE_ELEM(i,flist))

#define VANE_PP_DUCKINTERFACE_T_DUCK_DECL_ITEM_I(i, decl) \
    VANE_PP_IF( VANE_PP_IS_PAREN(decl) ) ( VANE_PP_DUCKINTERFACE_T_DUCK_DECL_ITEM_FUNC1,VANE_PP_EAT)(i, decl) 

#define VANE_PP_DUCKINTERFACE_T_DUCK_DECL_ITEM_FUNC1(i, seq) \
        VANE_PP_DUCKINTERFACE_DECLITEM_APPLY(VANE_PP_DUCKINTERFACE_T_DUCK_DECL_ITEM_FUNC1_DO, dummy,seq)

#define VANE_PP_DUCKINTERFACE_T_DUCK_DECL_ITEM_FUNC1_DO(dummy, attr, decl,...) \
        VANE_PP_EXPAND attr\
        VANE_PP_TUPLE_ELEM(1,decl) \
        VANE_PP_TUPLE_ELEM(0,decl) \
        (VANE_PP_REPEAT(VANE_PP_TUPLE_SIZE_EC(VANE_PP_TUPLE_ELEM(2,decl)), VANE_PP_DUCKINTERFACE_DECL_BASE_ARG1, VANE_PP_TUPLE_ELEM(2,decl)))\
        __VA_ARGS__ override { \
            return _data->VANE_PP_TUPLE_ELEM(0,decl) \
            (VANE_PP_REPEAT(VANE_PP_TUPLE_SIZE_EC(VANE_PP_TUPLE_ELEM(2,decl)), VANE_PP_DUCKINTERFACE_DECL_CALL_ARG1, VANE_PP_TUPLE_ELEM(2,decl)) );\
        }

#define VANE_PP_NS_DETAIL_DUCK(__IF)    VANE_PP_CAT_(_detail_duck_,__IF)

#define VANE_PP_DUCKINTERFACE_DEF_DUCK(nspart, namepart, flist) \
    VANE_PP_DUCKINTERFACE_DEF_DUCK_1(\
        VANE_PP_WHEN( VANE_PP_IS_NOT_EMPTY(nspart) ) \
            ( VANE_PP_CAT_(VANE_PP_EXPAND_,VANE_PP_EXPAND_ nspart)),\
        VANE_PP_TUPLE_ELEM(0,namepart),\
        flist)


#define VANE_PP_DUCKINTERFACE_DEF_DUCK_1(__NS, ifname, flist) \
namespace vane {                                                                                    \
namespace VANE_PP_NS_DETAIL_DUCK(ifname) {                                                          \
VANE_PP_WHEN( VANE_PP_IS_NOT_EMPTY(__NS) ) (                                                        \
namespace __NS {                                                                                    \
)                                                                                                   \
                                                                                                    \
template<typename __IF>                                                                             \
class _duck_XX                                                                                      \
{                                                                                                   \
    template<typename T>                                                                            \
    struct __Duck : std::remove_cv<__IF>::type                                                      \
    {                                                                                               \
        T   *_data;                                                                                 \
                                                                                                    \
        __Duck()     : _data(nullptr)   { }                                                         \
        __Duck(T &t) : _data(&t)        { }                                                         \
                                                                                                    \
    public:                                                                                         \
        VANE_PP_EVAL(VANE_PP_REPEAT(VANE_PP_VA_COUNT flist, VANE_PP_DUCKINTERFACE_T_DUCK_DECL_ITEM, flist)) \
    };                                                                                              \
                                                                                                    \
    struct _Store : __IF {                                                                          \
        void *_data;                                                                                \
    };                                                                                              \
    std::array<char, sizeof(_Store)>    _store;                                                     \
    using OBJ_TYPE = __IF;                                                                          \
    OBJ_TYPE *_get_obj() const  { return reinterpret_cast<OBJ_TYPE*>(const_cast<std::array<char, sizeof(_Store)>*>(&_store)); }\
    OBJ_TYPE *_get_obj() const volatile { return reinterpret_cast<OBJ_TYPE*>(const_cast<std::array<char, sizeof(_Store)>*>(&_store)); }\
public:                                                                                             \
        struct bad_duck {                                                                           \
            VANE_PP_EVAL(VANE_PP_REPEAT(VANE_PP_VA_COUNT flist, VANE_PP_DUCKINTERFACE_BAD_DUCK_DECL_ITEM, flist,__NS::ifname)) \
        };                                                                                          \
                                                                                                    \
    template <typename T, typename                                                                  \
        =   typename std::enable_if<                                                                \
                   !std::is_base_of<_duck_XX<const          typename std::remove_cv<__IF>::type>, T>::value \
                && !std::is_base_of<_duck_XX<      volatile typename std::remove_cv<__IF>::type>, T>::value \
                && !std::is_base_of<_duck_XX<const volatile typename std::remove_cv<__IF>::type>, T>::value \
                && ( !std::is_const<T>::value    || std::is_const<__IF>::value )                    \
                && ( !std::is_volatile<T>::value || std::is_volatile<__IF>::value )                 \
            >::type>                                                                                \
    _duck_XX(T &t) {                                                                                \
        using __T = typename std::remove_cv<T>::type;                                               \
        new(&_store) __Duck<__T>(const_cast<__T&>(t));                                              \
    }                                                                                               \
                                                                                                    \
    template <typename T>                                                                           \
    _duck_XX(T *t) : _duck_XX(*t) { }                                                               \
                                                                                                    \
public:                                                                                             \
    _duck_XX() { }                                                                                  \
    _duck_XX(vane::bad_duck) {                                                                      \
        new(&_store) __Duck<bad_duck>();                                                            \
    }                                                                                               \
    _duck_XX(std::nullptr_t) {                                                                      \
        std::fill(_store.begin(), _store.end(), 0);                                                 \
    }                                                                                               \
    operator __IF&() const          { return *_get_obj(); }                                         \
    operator __IF*() const          { return  _get_obj(); }                                         \
    operator __IF&() const volatile { return *_get_obj(); }                                         \
    operator __IF*() const volatile { return  _get_obj(); }                                         \
                                                                                                    \
    operator bool() const          {                                                                \
        ptrdiff_t *p = (ptrdiff_t*)this;                                                            \
        return vane::reducer<sizeof(*this)/sizeof(*p)-1>::get(std::bit_or<ptrdiff_t>(), *p, p);     \
    }                                                                                               \
    operator bool() const volatile  {                                                               \
        ptrdiff_t *p = (ptrdiff_t*)this;                                                            \
        return vane::reducer<sizeof(*this)/sizeof(*p)-1>::get(std::bit_or<ptrdiff_t>(), *p, p);     \
    }                                                                                               \
                                                                                                    \
    VANE_PP_EVAL(VANE_PP_REPEAT(VANE_PP_VA_COUNT flist, VANE_PP_DUCKINTERFACE_BASE_DUCK_DECL_ITEM, flist))  \
};                                                                                                  \
VANE_PP_WHEN( VANE_PP_IS_NOT_EMPTY(__NS) ) (                                                        \
}                                                                                                   \
)                                                                                                   \
}                                                                                                   \
    template<>                                                                                      \
        struct duck<__NS::ifname> : vane::VANE_PP_NS_DETAIL_DUCK(ifname)::__NS VANE_PP_WHEN( VANE_PP_IS_NOT_EMPTY(__NS) )(::)_duck_XX<__NS::ifname> { \
            using _duck_XX::_duck_XX;                                                               \
            operator       duck<const          __NS::ifname>&()       { return *reinterpret_cast<      duck<const          __NS::ifname>*>(this); } \
            operator const duck<const          __NS::ifname>&() const { return *reinterpret_cast<const duck<const          __NS::ifname>*>(this); } \
            operator       duck<      volatile __NS::ifname>&()       { return *reinterpret_cast<      duck<      volatile __NS::ifname>*>(this); } \
            operator const duck<      volatile __NS::ifname>&() const { return *reinterpret_cast<const duck<      volatile __NS::ifname>*>(this); } \
            operator       duck<const volatile __NS::ifname>&()       { return *reinterpret_cast<      duck<const volatile __NS::ifname>*>(this); } \
            operator const duck<const volatile __NS::ifname>&() const { return *reinterpret_cast<const duck<const volatile __NS::ifname>*>(this); } \
        };                                                                                          \
    template<>                                                                                      \
        struct duck<const __NS::ifname> : vane::VANE_PP_NS_DETAIL_DUCK(ifname)::__NS VANE_PP_WHEN( VANE_PP_IS_NOT_EMPTY(__NS) )(::)_duck_XX<const __NS::ifname> {\
            using _duck_XX::_duck_XX;                                                               \
            operator       duck<const volatile __NS::ifname>&()       { return *reinterpret_cast<      duck<const volatile __NS::ifname>*>(this); } \
            operator const duck<const volatile __NS::ifname>&() const { return *reinterpret_cast<const duck<const volatile __NS::ifname>*>(this); } \
        };                                                                                          \
    template <>                                                                                     \
        struct duck<volatile __NS::ifname> : vane::VANE_PP_NS_DETAIL_DUCK(ifname)::__NS VANE_PP_WHEN( VANE_PP_IS_NOT_EMPTY(__NS) )(::)_duck_XX<volatile __NS::ifname> {\
            using _duck_XX::_duck_XX;                                                               \
            operator       duck<const volatile __NS::ifname>&()       { return *reinterpret_cast<      duck<const volatile __NS::ifname>*>(this); } \
            operator const duck<const volatile __NS::ifname>&() const { return *reinterpret_cast<const duck<const volatile __NS::ifname>*>(this); } \
        };                                                                                          \
    template <>                                                                                     \
        struct duck<const volatile __NS::ifname> : vane::VANE_PP_NS_DETAIL_DUCK(ifname)::__NS VANE_PP_WHEN( VANE_PP_IS_NOT_EMPTY(__NS) )(::)_duck_XX<const volatile __NS::ifname> {\
            using _duck_XX::_duck_XX;                                                               \
        };                                                                                          \
}/*namespace vane/////////////////////////////////////////////////////////////////// */


namespace vane {/////////////////////////////////////////////////////////////////////////////////////////////
namespace __gstack2 {/////////////////////////////////////////////////////////////////////////////////////////////
//gstack2
//subject to change soon

struct gstack_default_traits
{
    enum {
        INITIAL_ANCHOR_CHUNK_SIZE   = 1,
        INITIAL_DATA_CHUNK_SIZE     = 1024,
    };
};

template<typename __Traits=gstack_default_traits, typename __Alloc=std::allocator<char>>
struct _GStack;

using gstack = _GStack<gstack_default_traits, std::allocator<char>>;



template<typename __GStack>
struct _Anchor
{
    __GStack    *_gstack;
    uint32_t    _index;
    _Anchor     *_owner;
    _Anchor     *_prev;
    void        *_data;

    uint32_t chunk_index() const {
        return _index >> (32-5);
    }

    uint32_t local_index() const {
        return _index & ((1U << (32-5)) - 1);
    }
};


template<typename __GStack>
struct _AnchorChunk
{
    using _Anchor = ::_Anchor<__GStack>;

    _AnchorChunk    *_prev;
    _Anchor         _anchors[];

    static
    size_t allocated_size(size_t cnt) {
        return sizeof(_AnchorChunk) + cnt*sizeof(_Anchor);
    }
};


template<typename __GStack, typename __Alloc = std::allocator<char>>
class _AnchorStack : private __Alloc
{
    using _Anchor       = ::_Anchor<__GStack>;
    using _AnchorChunk = ::_AnchorChunk<__GStack>;

    __GStack        *_gstack;
    unsigned        _top;
    unsigned        _limit;
    uint32_t        _index;
    _AnchorChunk    *_chunk;
    _AnchorChunk    *_free_chunks;
    _Anchor         *_frame;

public:
    using allocator_type    =   __Alloc;
    using _Chunk            = _AnchorChunk;

    _AnchorStack(__GStack *gstack, size_t initial_index=1)
        : _gstack(gstack)
        , _top(0), _limit(1 << initial_index), _index(initial_index)
        , _chunk( allocate_chunk(_limit) ), _free_chunks(nullptr)
        , _frame(nullptr)
    { }
    _AnchorStack(__GStack *gstack, size_t initial_index, __Alloc &alloc)
        : __Alloc(alloc)
        , _gstack(gstack)
        , _top(0), _limit(1 << initial_index), _index(initial_index)
        , _chunk( allocate_chunk(initial_index) ), _free_chunks(nullptr)
        , _frame(nullptr)
    { }

    ~_AnchorStack()
    {
        _Chunk *chunk = _chunk;
        _Chunk *prev  = _chunk;
        unsigned cnt = _limit;
        for(; chunk ; chunk=prev, cnt/=2 ) {
            prev = prev->_prev;
            deallocate_chunk(chunk, cnt);
        }

        cnt = _limit*2;
        chunk = _free_chunks;
        prev  = _free_chunks;
        for(; chunk ; chunk=prev, cnt*=2 ) {
            prev = prev->_prev;
            deallocate_chunk(chunk, cnt);
        }
    }


    _Chunk *allocate_chunk( unsigned cnt, _Chunk *prev=nullptr )
    {
        _Chunk *chunk = (_Chunk*)get_allocator().allocate( _chunk->allocated_size(cnt) );
                chunk->_prev  = prev;
        return chunk;
    }

    void deallocate_chunk( _Chunk *chunk, unsigned cnt )
    {
        get_allocator().deallocate((char*)chunk, chunk->allocated_size(cnt));
    }

    __Alloc &get_allocator() {
        return *this;
    }

    void shift_chunk()
    {
        unsigned new_index = _index + 1;
        unsigned new_limit = 2 * _limit;

        _Chunk *new_chunk;
        if( _free_chunks ) {
            new_chunk    = _free_chunks;
            _free_chunks = _free_chunks->_prev;
        }
        else {
            new_chunk = allocate_chunk(new_limit);
        }

        new_chunk->_prev = _chunk;
        _chunk = new_chunk;

        _top = 0;
        _limit = new_limit;
        _index = new_index;
    }

    void unshift_chunk()
    {
        _Chunk *old_chunk = _chunk;
        _chunk = old_chunk->_prev;

        old_chunk->_prev = _free_chunks;
        _free_chunks = old_chunk;

        _index -= 1;
        _limit >>= 1;
        _top = _limit;
    }

    unsigned make_anchor_index(unsigned index) {
        return (_index << (32-5)) | index;
    }

    _Anchor &push_anchor(void *buf)
    {
        if( _top >= _limit ) {
            shift_chunk();
        }

        _Anchor *anchor = &_chunk->_anchors[_top];
        anchor->_gstack = _gstack;
        anchor->_index  = make_anchor_index(_top);
        anchor->_owner  = anchor;
        anchor->_prev   = _frame;
        anchor->_data   = buf;
        _frame = anchor;

        ++_top;

        return *anchor;
    }

    _Anchor &get_frame() {
        return *_frame;
    }
};



struct _DataChunk
{
    _DataChunk  *_prev;
    unsigned    _chunk_size;
    char        _data[];

    unsigned capacity() {
        return _chunk_size - sizeof(_DataChunk);
    }

    void *data_begin() {
        return &_data[0];
    }

    void *data_end() {
        return (char*)this + _chunk_size;
    }

    bool having(void *pos)
    {
        return (uintptr_t)((char*)pos - (char*)this) < (uintptr_t)_chunk_size;
    }
};


template<typename __Traits, typename __Alloc>
struct _GStack : private __Alloc
{
    using __GStack      = _GStack;
    using _Anchor       = ::_Anchor<__GStack>;
    using _AnchorChunk = ::_AnchorChunk<__GStack>;
    using _AnchorStack  = ::_AnchorStack<__GStack,__Alloc>;

    _AnchorStack    _anchor_stack;
    void            *_top;
    void            *_limit;
    _DataChunk      *_data_chunk;
    _DataChunk      *_free_chunk;
    unsigned        _extra_size;

public:
    _GStack() 
        : _anchor_stack(this, (size_t)__Traits::INITIAL_ANCHOR_CHUNK_SIZE)
        , _data_chunk( allocate_chunk(__Traits::INITIAL_DATA_CHUNK_SIZE) )
        , _free_chunk( nullptr )
        , _extra_size(0)
    {
        _anchor_stack.push_anchor( &_data_chunk->_data[0] );
        _top   = _data_chunk->data_begin();
        _limit = _data_chunk->data_end();
    }

    _GStack(__Alloc &a) 
        : __Alloc(a)
        , _anchor_stack(this, __Traits::INITIAL_ANCHOR_CHUNK_SIZE, a)
        , _data_chunk( allocate_chunk(__Traits::INITIAL_DATA_CHUNK_SIZE) )
        , _free_chunk( nullptr )
        , _extra_size(0)
    {
        _anchor_stack.push_anchor( &_data_chunk->_data[0] );
        _top   = _data_chunk->data_begin();
        _limit = _data_chunk->data_end();
    }


    __Alloc &get_allocator() {
        return *this;
    }

    _Anchor &get_frame() {
        return _anchor_stack.get_frame();
    }

    _DataChunk *allocate_chunk( unsigned chunk_size )
    {
        _DataChunk *chunk = (_DataChunk*)get_allocator().allocate( chunk_size );
        chunk->_chunk_size = chunk_size;

        return chunk;
    }

    void deallocate_chunk( _DataChunk *chunk )
    {
        get_allocator().deallocate((char*)chunk, chunk->_chunk_size);
    }

    void setback(_Anchor *to)
    {
        auto to_chunk = to->chunk_index();

        while( to_chunk < _anchor_stack._index ) {
            _anchor_stack.unshift_chunk();
        }
        _anchor_stack._top = to->local_index();


        unsigned sum = 0;
        _DataChunk *keep = nullptr;

        _DataChunk *dc   = _data_chunk;
        for(; !dc->having(to->_data) ; dc=dc->_prev) {
            if( keep ) {
                sum += keep->capacity();
                deallocate_chunk( keep );
            }
            keep = dc;
        }
        _data_chunk = dc;
        _top   = to->_data;
        _limit = dc->data_end();

        if( _extra_size || sum || (keep && _free_chunk) ) {
            if( keep ) {
                sum += keep->capacity();
                deallocate_chunk( keep );
            }
            if( _free_chunk ) {
                sum += _free_chunk->capacity();
                deallocate_chunk( _free_chunk );
                _free_chunk = nullptr;
            }
            _extra_size += sum;
        }
        else {
            if( keep )
                _free_chunk = keep;
        }
    }
};

}//namespace __gstack2///////////////////////////////////////////////////////////////////////////////////////
}//namespace vane////////////////////////////////////////////////////////////////////////////////////////////


#endif  //___VANE_H_20170719
// vim: ts=4
