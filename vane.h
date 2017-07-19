/*
VANE	2017 July 19


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
#ifndef	___VANE_H_20170719
#define	___VANE_H_20170719

#include <type_traits>
#include <array>
#include <algorithm>
#include <utility>
#include <assert.h>
#include <tuple>
#include <iterator>
#include <typeinfo>
#include<exception>
#include <unordered_map>
#include <bits/functional_hash.h>
#include <iostream>
#include <functional>
#include <vector>
#include <iostream>

namespace vane {/////////////////////////////////////////////////////////////////////////////////////////////


/*-----------------------------------------------------------------------------------
	type stuff
*/

template<typename T>
	constexpr std::conditional_t<std::__or_<std::is_same<T,char>,std::is_same<T,unsigned char>>::value,unsigned,  std::make_unsigned_t<T>> mask_msb = (std::make_unsigned_t<T>(-1)>>1)+1;



template<typename T, T...I,T...J> constexpr
	auto concat_iseq(std::integer_sequence<T,I...>,std::integer_sequence<T,J...>) {
		return std::integer_sequence<T,I...,J...>{};
	}
template<size_t...I,size_t...J> constexpr
	auto concat_index_sequence(std::index_sequence<I...> i,std::index_sequence<J...> j) {
		return concat_iseq<size_t>(i,j);
	}


#ifdef	_MSC_VER  ///////////////////////////////////////////
/*
	from gcc stl
*/
	template<typename...> using __void_t = void;

	template<typename...>
	struct __or_;

	template<>
	struct __or_<> : public std::false_type { };

	template<typename _B1>
	struct __or_<_B1>
	: public _B1
	{ };

	template<typename _B1, typename _B2>
	struct __or_<_B1, _B2>
	: public std::conditional<_B1::value, _B1, _B2>::type
	{ };

	template<typename _B1, typename _B2, typename _B3, typename... _Bn>
	struct __or_<_B1, _B2, _B3, _Bn...>
	: public std::conditional<_B1::value, _B1, __or_<_B2, _B3, _Bn...>>::type
	{ };

	template<typename...>
	struct __and_;

	template<>
	struct __and_<>
	: public std::true_type
	{ };

	template<typename _B1>
	struct __and_<_B1>
	: public _B1
	{ };

	template<typename _B1, typename _B2>
	struct __and_<_B1, _B2>
	: public std::conditional<_B1::value, _B2, _B1>::type
	{ };

	template<typename _B1, typename _B2, typename _B3, typename... _Bn>
	struct __and_<_B1, _B2, _B3, _Bn...>
	: public std::conditional<_B1::value, __and_<_B2, _B3, _Bn...>, _B1>::type
	{ };

	template<typename _Pp>
	struct __not_
	: public std::integral_constant<bool, !_Pp::value>
	{ };
#elif defined(__GNUC__) ////////////////////////////////////////////////////
	using std::__or_;
	using std::__and_;
	using std::__not_;

	using std::__void_t;
#endif /////////////////////////////////////////////////////////////////////


template <bool>	struct __bool_t { };
template<typename T,T...>
using void_v = void;


/*---------------------------------------------------------------------------------------------
	VANE_DEFINE__HAS_MEMBER(xxx)

	__has_member_xxx<T>:value
	__has_member_xxx_v<T>
*/
#define	VANE_DEFINE__HAS_MEMBER(member)					\
		template<typename T,typename=vane::void_v<void>>		\
		struct __has_member_##member : std::false_type { };	\
														\
		template<typename T>							\
		struct __has_member_##member<T, vane::void_v<size_t, sizeof( declval<T>().member)>> : std::true_type { };\
														\
		template<typename T>							\
		constexpr auto __has_member_##member##_v = __has_member_##member<T>::value;	


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
	multi_array_getAt(a,1,2,3)
	multi_array_getAt(a, make_array(1,2,3,4));
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
			assert( i <  N );
			return __multi_array_wrap<const T>::at(a[i], is...);
		}
	};
}//end __helper__

	template<typename MA, typename...Is>
	typename __helper__::__multi_array_wrap<MA>::Data &
	multi_array_getAt(MA &a, Is...is) {
		return __helper__::__multi_array_wrap<MA>::at(a, is...);
	};


		template <typename MA, typename TI, size_t...I>
		typename __helper__::__multi_array_wrap<MA>::Data &
		__ma_helper_get(MA &a, const std::array<TI,sizeof...(I)> &ai, std::index_sequence<I...>) {
			return multi_array_getAt(a, ai[I]...);
		}

	template <typename MA, typename AI>
	const typename __helper__::__multi_array_wrap<MA>::Data &
	multi_array_getAt(const MA &a, const AI &ai) {
		return __ma_helper_get(a, ai, std::make_index_sequence<multi_array_rank<MA>::value>());
	}

	template <typename MA, typename AI>
	typename __helper__::__multi_array_wrap<MA>::Data &
	multi_array_getAt(MA &a, const AI &ai) {
		return __ma_helper_get(a, ai, std::make_index_sequence<multi_array_rank<MA>::value>());
	}


/*--------------------------------------------
	multi_array_flat_indexer<decltype(a)>::get(1,2,3)
*/
	template<typename T>
	struct multi_array_flat_indexer {
		enum { size = 1 };
		using Data = T;

		static constexpr
		int get() { return 0; }
	};

	template<typename T, size_t N>
	struct multi_array_flat_indexer<std::array<T,N>> {
		using PrevT = T;
		using Prev  = multi_array_flat_indexer<T>;
		enum { size = N*Prev::size };
		using Data = typename Prev::Data;

		template<typename I,typename...Is>
		static constexpr
		int get(I i, Is...is) {
			return i*Prev::size + Prev::get(is...);
		}
	};
/*--------------------------------------------
	multi_array_regular<int, 5,2>  --> multi_array<int, 2,2,2,2,2>
*/
		template <typename T, size_t W, typename Seq>
		struct __multi_array_regular;

		template <typename T, size_t W, size_t...I>
		struct __multi_array_regular<T,W,std::index_sequence<I...>> {
			using type = multi_array<T,(W+I-I)...>;
		};

	template <typename T, size_t Dim, size_t W>
	using multi_array_regular = typename __multi_array_regular<T, W, std::make_index_sequence<Dim>>::type;

/*--------------------------------------------
	sized_pdata<int,10>

		static const array<int,10> a{...};
		static const __d = sized_pdata<int>(a.size(), &a[0]);
*/
	template<typename T, typename SIZE_T=unsigned>
	struct sized_pdata {
		SIZE_T	size;
		T		*data;
		constexpr sized_pdata(unsigned _size, T *_data) : size(_size), data(_data) { }
	};



/*--------------------------------------------
	AbstractDummy
*/
struct AbstractDummy	{ virtual ~AbstractDummy()=0; };
struct abstract_dummy	{ virtual ~abstract_dummy()=0; };



/*----------------------------------------------------------------*/
template<typename>
struct resolve_signature;

template<typename R, typename...Ts>
struct resolve_signature<R(Ts...)> {
	using return_type = R;
	using arg_types   = std::tuple<Ts...>;
};


/*----------------------------------------------------------------*/
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


/*------------------------------------------------------------------------------
	basetype_xxx
*/
	template<typename B, typename D>
	struct basetype_is_base_of
		: std::is_base_of<remove_pointer_or_reference_t<B>,remove_pointer_or_reference_t<D>> { };

	template<typename T>
	struct basetype_is_polymorphic
		: std::is_polymorphic<remove_pointer_or_reference_t<T>> { };


/*----------------------------------------------------------------------------------
	is_callable
*/

struct substitution_failure {}; // represent a failure to declare something

template<typename T>
struct substitution_succeeded : std::true_type { };

template<> 
struct substitution_succeeded<substitution_failure> : std::false_type { };

template<typename B, typename...X>
struct _Check_result_of_call : B {
	static auto check() -> decltype(B::_check(static_cast<typename B::_Tag*>(nullptr), std::declval<X>()...));
};
template<typename B>
struct _Check_result_of_call<B,void> : B {
	static auto check() -> decltype( B::_check(static_cast<typename B::_Tag*>(nullptr)) );
};


template<typename F>
struct result_of_call {
protected:
	struct _Tag;

	static substitution_failure _check(...);					// cannnot call F(...)

	template<typename...X>
	static auto _check(_Tag*,X...x) -> decltype(std::declval<F>()(x...));// can call F(...)
public:
	template<typename...X>
		using type = decltype( _Check_result_of_call<result_of_call,X...>::check() );
public:
	using func_t = F;
	template<typename...X>
	static auto check() -> decltype(  _Check_result_of_call<result_of_call,X...>::check() );
};

template<typename F,typename...X>
using result_of_call_t = typename result_of_call<F>::template type<X...>;


template<typename F,typename...X>
struct is_callable : substitution_succeeded<typename result_of_call<F>::template type<X...>> { };

template<typename F,typename...X>
constexpr bool is_callable_v() { return is_callable<F,X...>::value; }


template<typename F>
struct has_callable : substitution_succeeded<typename result_of_call<F>::template type<>> { };

template<typename F,typename...X>
struct has_callable<F(X...)> : substitution_succeeded<typename result_of_call<F>::template type<X...>> { };



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

	//------------------------------------------------------------
	template <size_t Depth, typename TI=int, typename F>	//u,l,f
	void foreach_cartesian(const std::array<TI,Depth> &_uppers, const std::array<TI,Depth> &_lowers, F f) {
		std::array<TI,Depth> a;
		__Foreach_cartesian<F,Depth,decltype(a), TI>::go(a, f, _uppers,_lowers);
	}
	template <size_t Depth, typename TI=int, typename F>	//max,l,f
	void foreach_cartesian(TI max, const std::array<TI,Depth> &lowers, F f) {
		std::array<TI,Depth> uppers;
		uppers.fill(max);

		foreach_cartesian(uppers, lowers, f);
	}
	template <size_t Depth, typename TI=int, typename F>	//U,f
	void foreach_cartesian(const std::array<TI,Depth> &uppers, F f) {
		foreach_cartesian(uppers, std::array<TI,Depth>{0,}, f);
	}
	template <size_t Depth, typename TI=int, typename F>	//max,f
	void foreach_cartesian(TI max, F f) {
		foreach_cartesian(max, std::array<TI,Depth>{0,}, f);
	}
	template <size_t Depth, typename TI=int, typename F>	//max,min, f
	void foreach_cartesian(TI max, TI min, F f) {
		std::array<TI,Depth> lowers;
		lowers.fill(min);

		foreach_cartesian(max, lowers, f);
	}
	template <size_t Depth, typename TI=int, typename F>	//U,min, f
	void foreach_cartesian(const std::array<TI,Depth> &uppers, TI min, F f) {
		std::array<TI,Depth> lowers;
		lowers.fill(min);

		foreach_cartesian(uppers, lowers, f);
	}



/*//////////////////////////////////////////////////////////////////////////////////////////////
	tuple utils
*/

/*---------------------------------------------------------------------------------------------
	select_from_sequence<k, 1,2,3,4...>::value
*/
	template<int K, int...I>  struct select_from_sequence;

	template<int K, int I0, int...I>
	struct select_from_sequence<K, I0,I...> : select_from_sequence<K-1,I...> { };

	template<int I0, int... I>
	struct select_from_sequence<0, I0, I...> {
		enum { value=I0 };
	};

	template<int K, int...I>
	constexpr auto select_from_sequence_v = select_from_sequence<K,I...>::value;


/*---------------------------------------------------------------------------------------------
	reverse sequences:
		integer_rsequence<-1,-3,1...>::type
		integer_rsequence_t<-1,2,3...>
		_reverse_integer_sequence( integer_sequence<int,1,3,7...>);
		make_index_rsequence<int N>
*/
	template<typename T, T...I> 
	struct integer_rsequence {
		template<size_t...J>
			using _indexed_rsequence = std::integer_sequence<T, select_from_sequence<sizeof...(J)-J-1,I...>::value...   >;

		template<size_t...J>
		static constexpr
		auto _index_to_rsequence(std::index_sequence<J...> j) {
			return _indexed_rsequence<J...>{};
		}
		using type = decltype( _index_to_rsequence(std::make_index_sequence<sizeof...(I)>{}) );
	};
	template<typename T, T...I>
	using integer_rsequence_t = typename integer_rsequence<T,I...>::type;

	template<typename T, T...I>
	constexpr auto _reverse_integer_sequence(std::integer_sequence<T,I...>) {
		return integer_rsequence_t<T,I...>{};
	}
	template<size_t N>
	using make_index_rsequence = decltype(_reverse_integer_sequence( std::make_index_sequence<N>{} ));

	template<size_t...I>
	using index_rsequence = integer_rsequence_t<size_t,I...>;


/*---------------------------------------------------------------------------------------------
	make_integer_sequence_NxI<int, 10,-1>
	make_index_sequence_NxI<10,1>
*/
		template<typename T, size_t N, size_t I, typename Seq>
		struct __make_integer_sequence_NxI;

		template<typename T, size_t N,size_t I, size_t...J>
		struct __make_integer_sequence_NxI<T,N,I,std::index_sequence<J...>> {
			using type = std::integer_sequence<T,(I+J-J)...>;
		};

	template<typename T, size_t N, size_t I>
	using make_integer_sequence_NxI = typename __make_integer_sequence_NxI<T,N,I,std::make_index_sequence<N>>::type;

	template<size_t N, size_t I>
	using make_index_sequence_NxI = typename __make_integer_sequence_NxI<size_t,N,I,std::make_index_sequence<N>>::type;


/*---------------------------------------------------------------------------------------------
	iseq<1,2,3>
	iseq_cat_t<iseq<1,2,3>, iseq<4,5,6>>
	iseq_add_t<iseq<1,2,3>, -1,-2,1,2>
*/

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

//iseq_add_t<iseq<1,2,3>,11,22,33>
		template<typename T, int...I>
		struct iseq_add;
		template<int...Ti, int...Ui>
		struct iseq_add<iseq<Ti...>, Ui...> {
			using type = iseq<Ti...,Ui...>;
		};
	template<typename T, int...U>
	using iseq_add_t = typename iseq_add<T,U...>::type;


/*---------------------------------------------------------------------------------------------
	iseq_indexOf<1,iseq<1,2,3>, true/false>
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
	iseq_size< iseq<...> >
*/
#if 1	//XXX: use iseq<...>::size() instead
		template <typename Seq>
		struct __iseq_size;
		template <int...I>
		struct __iseq_size<iseq<I...>> : std::integral_constant<size_t, sizeof...(I)> {};
	template <typename Seq>
	constexpr size_t iseq_size = __iseq_size<Seq>::value;
#endif


/*---------------------------------------------------------------------------------------------
	__T_value_v<T,int,3>	;--> T::value or (int)3
*/
			template<typename T, typename V, V val, typename=void_v<V>>
			struct __T_value : std::integral_constant<V,val> {};

			template< typename T, typename V, V val>
			struct __T_value<T,V,val, void_v<V,T::value>> : std::integral_constant<decltype(T::value),T::value> {};

		template<typename T, typename V, V val>
		constexpr auto __T_value_v = __T_value<T,V,val>::value;


/*---------------------------------------------------------------------------------------------
	__T_map_t<T, void>	;--> typename T::map or void
*/
		template<typename T, typename X, typename=__void_t<X>>
		struct __T_map { using map = X; };

		template< typename T, typename X>
		struct __T_map<T,X,__void_t<typename T::map>> { using map = typename T::map; };

	template<typename T, typename X>
	using __T_map_t = typename __T_map<T,X>::map;


/*---------------------------------------------------------------------------------------------
	__T_type_t<T, void>	;--> typename T::map or void
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
	template<typename ISeq, template<int SI, size_t I, typename _ISeq, typename...Params> class Filter, typename Indexer=std::make_index_sequence<iseq_size<ISeq>>, typename...FilterParam>
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
				__T_map_t < Filter<iseq_at<I,ISeq>,I,ISeq,FilterParam...>,  iseq<iseq_at<I,ISeq>> > //iseq<iseq_at<I,ISeq>>
			>::type...
		>;
	};

	template<typename ISeq, template<int SI, size_t I, typename _ISeq, typename...Params> class Filter, typename...FilterParam>
	using iseq_map = typename __iseq_map<ISeq, Filter, std::make_index_sequence<iseq_size<ISeq>>, FilterParam...>::type;

	template<typename ISeq, template<int SI, size_t I, typename _ISeq, typename...Params> class Filter, typename...FilterParam>
	using iseq_map2 = __iseq_map<ISeq, Filter, std::make_index_sequence<iseq_size<ISeq>>, FilterParam...>;

/*---------------------------------------------------------------------------------------------
	iseq_selectAt<iseq<0,1,2,3>, 1,3>	--> iseq<1,3>
	iseq_selectAt<iseq<0,1,2,3>>		--> iseq<>
	iseq_omitAt<iseq<0,1,2,3>, 1,3>	--> iseq<0,2>
	iseq_omitAt<iseq<0,1,2,3>>		--> iseq<0,1,2,3>
*/
		template <bool select, typename Seq, typename SiSeq, size_t...K>
		struct __iseq_selectAt;

		template <bool select, int...Si, int...I, size_t...K>
		struct __iseq_selectAt<select,iseq<Si...>, iseq<I...>, K...> {
			using type =iseq_cat_t<
							std::conditional_t<
								(0>iseq_indexOf<I,iseq<K...>,false>),
								std::conditional_t<select, iseq<>, iseq<Si>>,
								std::conditional_t<select, iseq<Si>, iseq<>>
							>...
						>;
		};
		template <bool select, int...I, size_t...K>
		struct __iseq_selectAt<select,iseq<>, iseq<I...>, K...> {
			using type =iseq<>;
		};
	template <typename Seq, size_t...K>
		using iseq_selectAt = typename __iseq_selectAt<true,Seq, std::make_integer_sequence<int,iseq_size<Seq>>,K...>::type;
	template <typename Seq, size_t...K>
		using iseq_omitAt = typename __iseq_selectAt<false,Seq, std::make_integer_sequence<int,iseq_size<Seq>>,K...>::type;

/*---------------------------------------------------------------------------------------------
	iseq_andAll<1,2,3>
	iseq_orAll<1,2,3>
*/
			template<typename T>
			struct __iseq_andAll;

			template<int...I>
			struct __iseq_andAll<iseq<I...>> 
				: __and_<  std::integral_constant<bool, (bool)I>... > { };

	template<typename T>
	constexpr bool iseq_andAll = __iseq_andAll<T>::value;


			template<typename T>
			struct __iseq_orAll;

			template<int...I>
			struct __iseq_orAll<iseq<I...>> 
				: __or_<  std::integral_constant<bool, (bool)I>... > { };

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
		using type = iseq<(I+Value-I)... >;
	};

	template<int N, int Value>
	using iseq_n = typename __iseq_n<Value, std::make_index_sequence<N>>::type;


/////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////
//#include <tuple>
/*---------------------------------------------------------------------------------------------
	reverse_tuple_t<Types...>
*/
			//helpers
			namespace __helpers__ {
				template <typename T>
				struct Reverse_tuple {
					using type = T;
				};

				template <typename T, typename...Rest>
				struct Reverse_tuple<std::tuple<T,Rest...>>  {
					using type = decltype(tuple_cat(std::declval<typename Reverse_tuple<std::tuple<Rest...>>::type>(), std::declval<std::tuple<T>>()));
				};
			}
	template <typename T>
	using reverse_tuple_t = typename __helpers__::Reverse_tuple<T>::type;
/*---------------------------------------------------------------------------------------------
	tuple_foreach	//ref: tuple-foreach.cc
		foreach(tupleData, tupleFuntions, FArgs...fargs);
			_foreach<1,2...>(tupleData, tupleFuntions, FArgs...fargs);
			_foreach(tupleData, tupleFuntions, index_sequence<1,3...>,FArgs...fargs);

		foreach<F>(tupleData, FArgs...fargs);
			_foreach<F,1,3...>(tupleData, FArgs...fargs);
			_foreach<F>(tupleData, index_sequence<1,3...>,FArgs...fargs);
*/
#ifndef	__clang__
				template<typename T,typename F, size_t...I, typename...FArgs>
				void ___foreach(T &&t, F &&f, std::index_sequence<I...>&&, FArgs...fargs) {
					std::tie((std::get<I>(f)(std::get<I>(t),fargs...),std::ignore)...);
				};
#endif
				//	_foreach( tuple, F[], <1,2..>,args...)	;helper
				template<typename T,typename F, size_t...I, typename...FArgs>
				void _foreach(T &&t, F &&f, std::index_sequence<I...>&& seq, FArgs...fargs) {
#ifdef	__clang__
					std::tie((std::get<I>(f)(std::get<I>(t),fargs...),std::ignore)...);
#else
					___foreach(t, f, _reverse_integer_sequence(seq), fargs...);
#endif
				};
	//	_foreach<1,2..>( tuple, F[], args...)
	template<size_t I0,size_t...I, typename T, typename F, typename...FArgs>
	void _foreach(T &&t, F &&f, FArgs...fargs) {
			_foreach(t, f, std::index_sequence<I0,I...>{}, fargs...);
	};
	//	foreach( tuple, F[], args...)
	template<typename T, typename F, typename...FArgs>
	void foreach(T &&t, F &&f, FArgs...fargs) {
		_foreach(t, f, std::make_index_sequence<std::tuple_size<typename std::remove_reference<T>::type>::value>{}, fargs...);
	};

//////////////////////////////////////////////////////////////////////////////////////////////
// foreach<F>( tuple, args... )
	////////with FArgs.../////////////////////////////////////////////////////////////////////
#ifndef	__clang__
			template<template<typename...> class F, size_t...I, typename T, typename... FArgs>
			void ___foreach(T &&t, std::index_sequence<I...>&&, FArgs...fargs) {
				std::tie(((F<decltype(std::get<I>(t))>{})(std::get<I>(t),fargs...),std::ignore)...);
			};
#endif
			//	_foreach<F>( tuple, <1,2..>, args...)
			template<template<typename...> class F, size_t...I, typename T, typename... FArgs>
			void _foreach(T &&t, std::index_sequence<I...>&&seq, FArgs...fargs) {
#ifdef	__clang__
				std::tie(((F<decltype(std::get<I>(t))>{})(std::get<I>(t),fargs...),std::ignore)...);
#else
				___foreach<F>(t, _reverse_integer_sequence(seq), fargs...);
#endif
			};
	//	_foreach<F, 1,2..>( tuple, args...)
	template<template<typename...> class F, size_t I0,size_t...I, typename T, typename... FArgs>
	void _foreach(T &&t, FArgs...fargs) {
		_foreach<F>(t, std::index_sequence<I0,I...>{}, fargs...);
	};
	//	foreach<F>( tuple, args...)
	template<template<typename...> class F, typename T, typename... FArgs> inline
	void foreach(T &&t, FArgs...fargs) {
		_foreach<F>(t, std::make_index_sequence<std::tuple_size<typename std::remove_reference<T>::type>::value>{}, fargs...);
	};
///////////////////////////////////////////////////////////////////////////////////////////////
/*---------------------------------------------------------------------------------------------
	tuple_index<T, tuple<...>, _assert_=true>
*/
		template <class T, class typelist, bool _assert_=true>
		struct ___tuple_index;
		//for tuple<>
			template <bool _assert_, class T, class...Types>
			struct ___tuple_index<T, std::tuple<T, Types...>, _assert_> {
				enum { value = 0 };
			};

			template <class T>
			struct ___tuple_index<T, std::tuple<>, false> {
				enum { value = -1 };
			};
////////////
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
	tuple_count_v<X, tuple<...>>
	tuple_count_v<X, tuple<>>
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
		tuple_n<3,int>
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
	//-------------------------------------------------
	template<typename Set>
	struct __tuple_catInner;

	template<typename...T>
	struct __tuple_catInner<std::tuple<T...>> {
		using type = tuple_cat_t<T...>;
	};

	template <typename...T>
	using tuple_catInner = typename __tuple_catInner<T...>::type;
/*---------------------------------------------------------------------------------------------
	tuple_add_t<tuple<>>
	tuple_add_t<tuple<>, X,Y,Z...>
*/
		template <typename...T>
		struct tuple_add;

		template <typename...T, typename...U>
		struct tuple_add<std::tuple<T...>,U...> {
			using type = tuple_cat_t<std::tuple<T...>, std::tuple<U>...>;
		};
	template <typename...T>
	using tuple_add_t = typename tuple_add<T...>::type;

/*---------------------------------------------------------------------------------------------
	tuple_union<>::type
		tuple_union<tuple<...>, int>
		tuple_union<tuple<...>, tuple<...>>
*/
		template <typename...T>
		struct tuple_union;

			template <typename T, typename U>
			struct tuple_union<T,U> {
				using type = typename tuple_union<T,std::tuple<U>>::type;
			};

			template <typename T, typename U>
			struct tuple_union<T,std::tuple<U>> {
				using type = 
					typename std::conditional<
						(0 > tuple_index<U,T,false>),
						decltype(  std::tuple_cat(	std::declval<T>(),
													std::declval<std::tuple<U>>())	),
						T
					>::type;
			};

			template <typename T, typename U, typename...Us>
			struct tuple_union<T,std::tuple<U,Us...>> {
				using type = 
					typename tuple_union<
						typename tuple_union<T,U>::type,
						std::tuple<Us...>
					>::type;
			};
		template <typename T,typename U, typename...V>
		struct tuple_union<T,U,V...> {
			using type = typename tuple_union< typename tuple_union<T,U>::type, V... >::type;
		};
	template<typename...T>
	using tuple_union_t = typename tuple_union<T...>::type;
/*---------------------------------------------------------------------------------------------
	tuple_uniq_t< tuple<...> >
		tuple_uniq_t<tuple<  tuple<A,B>,tuple<A,B,C>,tuple<A,B>   >>	-> tuple< tuple<A,B>, tuple<A,B,C>>
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
	tuple_setAt<1,tuple<...>>
*/
	template<size_t I, typename F, typename T, typename Seq>
	struct __tuple_setAt__;

	template<size_t I, typename F, typename T, typename TJ, TJ...J>
	struct __tuple_setAt__<I,F,T,std::integer_sequence<TJ,J...>> {
		using type = std::tuple< typename std::conditional<I==J, T, std::tuple_element_t<J,F> >::type... >;
	};

	template<size_t I, typename F, typename T=void>
	struct __tuple_setAt {
		using type = typename __tuple_setAt__<I,F,T, std::make_index_sequence< std::tuple_size<F>::value  >>::type;
	};

	template<size_t I, typename F, typename T=void>
	using tuple_setAt = typename __tuple_setAt<I,F,T>::type;


/*---------------------------------------------------------------------------------------------
	tuple_contains<tuple<A,B,C>, tuple<A,B>>;
	tuple_contains<tuple<A,B,C>, tuple<>>;
*/
		template<typename T, typename U>
		struct __tuple_contains;

		template<typename T, typename...U>
		struct __tuple_contains<T,std::tuple<U...>>
			: std::integral_constant<bool,  iseq_andAll<iseq<(0<=tuple_index<U,T,false>)...>> > {
		};
		template<typename T>
		struct __tuple_contains<T,std::tuple<>>
			: std::integral_constant<bool,  true> {
		};
	template<typename T, typename U>
	constexpr bool tuple_contains = __tuple_contains<T,U>::value;


/*---------------------------------------------------------------------------------------------
	tuple_sort_t<tuple<Sub,Sub>, tuple<Whole,Sub>>
*/
		template <typename T,typename Order, typename=void>
		struct __tuple_sort;

		template <typename T,typename...O>
		struct __tuple_sort<T,std::tuple<O...>>
			: std::enable_if<tuple_contains<std::tuple<O...>,T>, tuple_cat_t< tuple_n<tuple_count_v<O, T>, O>... > > {};

	template<typename T, typename Order>
		using tuple_sort_t = typename __tuple_sort<T,Order>::type;

/*---------------------------------------------------------------------------------------------
	tuple_selectAt<tuple<int,char,float,long,int>, 1,3> --->	tuple<char, long>
*/

		template<bool select, typename T, typename Seq, size_t...K>
		struct __tuple_selectAt;

		template<bool select, typename T, size_t...I, size_t...K>
		struct __tuple_selectAt<select, T, std::integer_sequence<size_t,I...>, K...> {
			using type = tuple_cat_t< 
				std::tuple<>,
				std::conditional_t<
					(0>iseq_indexOf<I,iseq<(int)K...>,false>),
					std::conditional_t<select, std::tuple<>, std::tuple<std::tuple_element_t<I,T>>>,
					std::conditional_t<select, std::tuple<std::tuple_element_t<I,T>>, std::tuple<>>
				>...
			>;
		};
	template<typename T, size_t...K>
		using tuple_selectAt = typename __tuple_selectAt<true, T, std::make_index_sequence<std::tuple_size<T>::value>, K...>::type;
	template<typename T, size_t...K>
		using tuple_omitAt = typename __tuple_selectAt<false, T, std::make_index_sequence<std::tuple_size<T>::value>, K...>::type;
/*---------------------------------------------------------------------------------------------
	tuple_permute<tuple<A,B,C>>;
*/
		template <typename T, typename Seq=std::make_index_sequence<std::tuple_size<T>::value>, typename G=std::tuple<>>
		struct __tuple_permute;

		template <typename...Ts, size_t...I, typename G>
		struct __tuple_permute<std::tuple<Ts...>,std::index_sequence<I...>,G> {
			using type = tuple_cat_t<typename __tuple_permute< tuple_omitAt<std::tuple<Ts...>,I>, std::make_index_sequence<sizeof...(Ts)-1>, tuple_add_t<G,Ts> >::type...  >;
		};

		template <typename G>
		struct __tuple_permute<std::tuple<>,std::index_sequence<>,G> {
			using type = std::tuple<G>;
		};

	template <typename T>
	using tuple_permute = typename __tuple_permute<T>::type;
/*---------------------------------------------------------------------------------------------
	tuple_cartesian_product<tuple<tuple<A,B>,tuple<X,Y>>>
*/
	namespace __vane_helper__ {
		template <typename Tuples, typename Prefix=std::tuple<>>
		struct __cprod;

		template <typename Prefix, typename...Ts, typename...Us>
		struct __cprod<std::tuple<std::tuple<Ts...>, Us...>, Prefix> {
			using type = tuple_cat_t< typename __cprod<std::tuple<Us...>, tuple_add_t<Prefix,Ts>>::type... >;
		};
		template <typename Prefix>
		struct __cprod<std::tuple<>, Prefix> {
			using type = std::tuple<Prefix>;
		};
	}
	template <typename Tuples>
	using tuple_cartesian_product = typename __vane_helper__::__cprod<Tuples>::type;

/*---------------------------------------------------------------------------------------------
	tuple_map<tuple<...>, afilter, filterArgs...>;

			template<typename Ti, size_t I, typename Tuple, typename...FilterParam>
			struct afilter
				: integral_constant<int, (I>=tuple_size<Tuple>::value/2)> { using map=tuple<Ti,Ti,Ti>; };
		
			tuple_map<tuple<...>, afilter, filterArg...>;
*/
		template<typename Tuple, template<typename,size_t,typename,typename...> class Filter, typename Seq=std::make_index_sequence<std::tuple_size<Tuple>::value>, typename...FilterParam>
		struct __tuple_map;

		template<typename Tuple, template<typename,size_t,typename,typename...> class Filter, typename...FilterParam, size_t...I>
		struct __tuple_map<Tuple, Filter, std::index_sequence<I...>, FilterParam...> {
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
					//XXX//__T_map_t  <Filter<std::tuple_element_t<I,Tuple>,I,Tuple,FilterParam...>,  std::tuple<typename std::tuple_element<I,Tuple>::type>>,
				>::type...
			>;
		};

	template<typename Tuple, template<typename,size_t,typename,typename...> class Filter,typename...FilterParam>
	using tuple_map = typename __tuple_map<Tuple,Filter,std::make_index_sequence<std::tuple_size<Tuple>::value>,FilterParam...>::type;

	template<typename Tuple, template<typename,size_t,typename,typename...> class Filter,typename...FilterParam>
	using tuple_map2 = __tuple_map<Tuple,Filter,std::make_index_sequence<std::tuple_size<Tuple>::value>,FilterParam...>;

/*---------------------------------------------------------------------------------------------
	tuple_mapv<tuple<A,B,C,D>, aMapper, filterArg>;
*/
		template<typename Tuple, template<typename,size_t,typename,typename...> class Mapper, typename Seq=std::make_index_sequence<std::tuple_size<Tuple>::value>, typename...FilterParam>
		struct __tuple_mapv;

		template<typename Tuple, template<typename,size_t,typename,typename...> class Mapper, typename...FilterParam, size_t...I>
		struct __tuple_mapv<Tuple, Mapper, std::index_sequence<I...>, FilterParam...> {
			using type = iseq_cat_t<
				__T_map_t  <
					Mapper<std::tuple_element_t<I,Tuple>,I,Tuple,FilterParam...>,
					iseq< __T_value_v< Mapper< std::tuple_element_t<I,Tuple>,I,Tuple,FilterParam...>, int, 0> >
					/*msc:
						\work\test-misc\my\tuple_foreach.h|974| fatal error C1001: An internal error has occurred in the compiler.
						|| (compiler file 'f:\dd\vctools\compiler\cxxfe\sl\p1\c\convert.cpp', line 778)
						||  To work around this problem, try simplifying or changing the program near the locations listed above.
						|| Please choose the Technical Support command on the Visual C++ 
						||  Help menu, or open the Technical Support help file for more information
					*/
				>...
			>;
		};

	template<typename Tuple, template<typename,size_t,typename,typename...> class Mapper,typename...FilterParam>
	using tuple_mapv = typename __tuple_mapv<Tuple,Mapper,std::make_index_sequence<std::tuple_size<Tuple>::value>,FilterParam...>::type;


/*---------------------------------------------------------------------------------------------
	tuple_all_of<tuple<A,B,C,D>, afilter, filterparams...>
	tuple_any_of<tuple<A,B,C,D>, afilter, filterparams...>
*/
		template<typename T,template<typename,size_t,typename,typename...>class Filter, typename...Param>
		struct __tuple_all_of;

		template<template<typename,size_t,typename,typename...>class Filter, typename...T, typename...Param>
		struct __tuple_all_of<std::tuple<T...>,Filter,Param...>
			: std::integral_constant<bool, sizeof...(T)==std::tuple_size< tuple_map< std::tuple<T...>, Filter, Param... > >::value > { };

	template<typename T,template<typename,size_t,typename,typename...>class Filter, typename...Param>
	constexpr bool tuple_all_of = __tuple_all_of<T,Filter,Param...>::value;

		template<typename T,template<typename,size_t,typename,typename...>class Filter, typename...Param>
		struct __tuple_any_of;

		template<template<typename,size_t,typename,typename...>class Filter, typename...T, typename...Param>
		struct __tuple_any_of<std::tuple<T...>,Filter,Param...>
			: std::integral_constant<bool, !!std::tuple_size< tuple_map< std::tuple<T...>, Filter, Param... > >::value > { };

	template<typename T,template<typename,size_t,typename,typename...>class Filter, typename...Param>
	constexpr bool tuple_any_of = __tuple_any_of<T,Filter,Param...>::value;



/*---------------------------------------------------------------------------------------------
	iseq_filters
*/
	namespace iseq_filters {
		template<int Si, size_t I, typename ISeq, typename Tuples>
		struct map_tuple_size
			{ using map = iseq< std::tuple_size<std::tuple_element_t<I,Tuples>>::value >; };
	
		template<int SI, size_t I, typename ISeq>
		struct map_nonZero_into_index	{ using map = iseq< SI ? I : 0>; };

		template<int SI, size_t I, typename ISeq>
		struct filter_nonZero_into_index : std::integral_constant<bool, SI> { using map = iseq<I>; };

		template<int SI, size_t I, typename ISeq, typename V>
		struct filter_value_into_index : std::integral_constant<bool, SI==V::value> { using map = iseq<I>; };

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



/*---------------------------------------------------------------------------------------------
	iseq_inc<S, delta>::type;
	iseq_scale<S, x>::type;
	iseq_plus<A,B>::type
	iseq_mul<A,B>::type
	iseq_div<A,B>::type
	iseq_partial_sum<A>
*/
	template<typename ISeq, int D>
	struct iseq_inc;

	template<int...I, int D>
	struct iseq_inc<iseq<I...>,D> {
		using type = iseq<(I+D)...>;
	};

	////////////////////////////////////////////////////////////////
	template<typename ISeq, int S>
	struct iseq_scale;

	template<int...I, int S>
	struct iseq_scale<iseq<I...>,S> {
		using type = iseq<(I*S)...>;
	};

	////////////////////////////////////////////////////////////////
	template<typename SeqA, typename SeqB>
	struct iseq_plus;

	template<int...I, int...J>
	struct iseq_plus<iseq<I...>,iseq<J...>> {
		using type = iseq<(I+J)...>;
	};

	////////////////////////////////////////////////////////////////
	template<typename SeqA, typename SeqB>
	struct iseq_mul;

	template<int...I, int...J>
	struct iseq_mul<iseq<I...>,iseq<J...>> {
		using type = iseq<(I*J)...>;
	};

	////////////////////////////////////////////////////////////////
	template<typename SeqA, typename SeqB>
	struct iseq_div;

	template<int...I, int...J>
	struct iseq_div<iseq<I...>,iseq<J...>> {
		using type = iseq<(I/J)...>;
	};

	////////////////////////////////////////////////////////////////
	template<typename ISeq, typename G=iseq<0>, int S=0>
	struct _ISeq_partial_sum;

	template<int I0, int...I, typename G>
	struct _ISeq_partial_sum<iseq<I0, I...>, G> {
		using next = _ISeq_partial_sum<iseq<I...>, iseq_add_t<G, iseq_at<iseq_size<G>-1,G>+I0>>;
		using type = typename next::type;
	};

	template<typename G>
	struct _ISeq_partial_sum<iseq<>, G> {
		using type = G;
	};

	template<typename ISeq>
	using iseq_partial_sum = iseq_omitAt<typename _ISeq_partial_sum<ISeq>::type,0>;


/*---------------------------------------------------------------------------------------------
	tuple_filter_is_base_of   <Ti,I,Tuple,D>
	tuple_filter_is_derived_of<Ti,I,Tuple,B>

	is_base_of_all   <X,tuple<D...>>
	is_derived_of_all<X,tuple<B...>>
*/
	namespace tuple_filters {
		template<typename T, size_t I, typename Tuple, typename D>
		struct is_base_of: std::is_base_of<T,D> { };

		template<typename T, size_t I, typename Tuple, typename B>
		struct is_derived_from: std::is_base_of<B,T> { };

		template<typename T, size_t I, typename Tuple, typename X>
		struct is_same: std::is_same<T,X> { };

		template<typename T, size_t I, typename Tuple, typename X>
		struct is_not_same: std::integral_constant<bool, !std::is_same<T,X>::value > { };

		template<typename T, size_t I, typename Tuple, typename D>
		struct basetype_is_base_of: vane::basetype_is_base_of<T,D> { };

		template<typename T, size_t I, typename Tuple, typename B>
		struct basetype_is_derived_from: vane::basetype_is_base_of<B,T> { };
	}

	//-------------------------------------------------------------------------
	template<typename T, typename D>
	struct __is_base_of_all;

	template<typename T, typename...D>
	struct __is_base_of_all<T,std::tuple<D...>>
		: std::integral_constant<bool, tuple_all_of<std::tuple<D...>,tuple_filters::is_derived_from, T> > { };

	template<typename T>

	struct __is_base_of_all<T,std::tuple<>>;

	template<typename T, typename D>
	constexpr bool is_base_of_all = __is_base_of_all<T,D>::value;


			template<typename T, typename B>
			struct __is_derived_of_all;

			template<typename T>
			struct __is_derived_of_all<T,std::tuple<>>;

			template<typename T, typename...B>
			struct __is_derived_of_all<T,std::tuple<B...>>
				: std::integral_constant<bool, tuple_all_of<std::tuple<B...>,tuple_filters::is_base_of, T> > { };

	template<typename T, typename B>
	constexpr bool is_derived_of_all = __is_derived_of_all<T,B>::value;

	namespace tuple_filters {
		template<typename T, size_t I, typename Tuple, typename TypeList>
		struct is_base_of_all: std::integral_constant<bool, vane::is_base_of_all<T, TypeList> > { };

		template<typename T, size_t I, typename Tuple, typename TypeList>
		struct is_derived_of_all: std::integral_constant<bool, vane::is_derived_of_all<T, TypeList> > { };
	}

	template <typename...T>
	struct common_base {
		using types = std::tuple<T...>;
		using type  = std::tuple_element_t<0,tuple_map<types, tuple_filters::is_base_of_all, types>>;
	};

	template <typename...T>
	using common_base_t = typename common_base<T...>::type;

	template <typename>
	struct common_base_of_packed;

		template <typename...T>
		struct common_base_of_packed<std::tuple<T...>> {
			using types = std::tuple<T...>;
			using type  = std::tuple_element_t<0,tuple_map<types, tuple_filters::is_base_of_all, types>>;
		};
		template <typename T>
		using common_base_of_packed_t = typename common_base_of_packed<T>::type;


	namespace tuple_filters {
		template<typename T, size_t I, typename Tuple, typename N>
		struct skipN : std::integral_constant<bool, (int(I)>= N::value) > { };

		template<typename T, size_t I, typename Tuple>
		struct map_tuple_wrapped { using map = std::tuple< std::tuple<T> >; };

		template<typename X, size_t I, typename T>
		struct map_add_pointer
			{ using map = std::tuple<std::add_pointer_t<X>>; };

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
		struct map_remove_pointer_or_reverence {
			using map = std::tuple< remove_pointer_or_reference_t<X> >;
		};
	}
	template<typename T, int N>
	using tuple_skip_n = tuple_map<T, tuple_filters::skipN, std::integral_constant<int, N>>;

	//add_xxx
	template<typename Tuple>
	using add_pointers = tuple_map<Tuple, tuple_filters::map_add_pointer>;

	template<typename Tuple>
	using add_references = tuple_map<Tuple, tuple_filters::map_add_reference>;

	template<typename Tuple>
	using add_rvalue_references = tuple_map<Tuple, tuple_filters::map_add_rvalue_reference>;

	//remove_xxx
	template<typename Tuple>
	using remove_pointers = tuple_map<Tuple, tuple_filters::map_remove_pointer>;

	template<typename Tuple>
	using remove_references = tuple_map<Tuple, tuple_filters::map_remove_reference>;

	template<typename Tuple>
	using remove_pointers_or_references = tuple_map<Tuple, tuple_filters::map_remove_pointer_or_reverence>;


/*---------------------------------------------------------------------------------------------
	tuple_indices<tuple<A,A,C,D>, tuple<A,B,C,D>>	--> iseq<0,0,3,4>
*/
	template <typename L,typename R, typename _assert_=std::true_type, typename _default_=std::integral_constant<int,-1>>
	struct __tuple_indices {
		using type = tuple_mapv<L, iseq_filters::map_index, R,_assert_,_default_>;
	};
	template <typename L,typename R, typename _assert_=std::true_type, typename _default_=std::integral_constant<int,-1>>
	using tuple_indices = typename __tuple_indices<L,R,_assert_,_default_>::type;





/*---------------------------------------------------------------------------------------------------
	class_tree
*/

namespace class_tree {///////////////////////////////////////////////////////////////////////////////////
/*
	struct inheritance_groups;
*/
template<typename Elements, typename Groups=std::tuple<>>
struct inheritance_groups;

	template<typename T, size_t I, typename Tuple, typename New>
	struct filter_group_sub_of: std::integral_constant<bool, std::is_base_of<New,std::tuple_element_t<0,T>>::value> { };

	template<typename T, size_t I, typename Tuple, typename New>
	struct filter_group_not_sub_of: std::integral_constant<bool, !std::is_base_of<New,std::tuple_element_t<0,T>>::value> { };

	template<typename T, size_t I, typename Tuple, typename New>
	struct filter_group_base_of: std::integral_constant<bool, std::is_base_of<std::tuple_element_t<0,T>,New>::value> { };

	template<typename T, size_t I, typename Tuple, typename New>
	struct mapper_addNewSub {
		using map = std::tuple<std::conditional_t<
			!!std::is_base_of<std::tuple_element_t<0,T>,New>::value,
			tuple_add_t<T,New>,
			T
		>>;
	};

	template<typename E,typename...Ei, typename...Gi>
	struct inheritance_groups<std::tuple<E,Ei...>, std::tuple<Gi...>> {
		using type = 
			typename inheritance_groups<
				std::tuple<Ei...>,
				std::conditional_t<
					!!std::tuple_size<tuple_map<std::tuple<Gi...>, filter_group_base_of, E>>::value,
					tuple_map<std::tuple<Gi...>, mapper_addNewSub, E>,
					std::conditional_t<
						!!std::tuple_size<tuple_map<std::tuple<Gi...>, filter_group_sub_of, E>>::value,
						tuple_cat_t<
							std::tuple<
								tuple_uniq_t<
									tuple_cat_t<
										std::tuple<E>,
										tuple_catInner<tuple_map<std::tuple<Gi...>, filter_group_sub_of, E>>
									>
								>
							>,
							tuple_map<std::tuple<Gi...>, filter_group_not_sub_of, E>
						>,
						std::tuple<Gi...,std::tuple<E>>
					>
				>
			>::type;
	};

	template<typename G>
	struct inheritance_groups<std::tuple<>, G> {
		using type = G;
	};

/*------------------------------------------------------------------------------------
	getBranch
*/
template<typename Elements, typename SLine=std::tuple<>>
struct getBranch;

	template<typename S>
	struct getBranch<std::tuple<>,S> {
		using type = S;
	};

	template<typename T,typename S>
	struct getBranch<std::tuple<T>,S> {
		using type = tuple_add_t<S,T>;
	};

	template<typename T,typename...Ri, typename S>
	struct getBranch<std::tuple<T,Ri...>,S> {
		using _groups = typename inheritance_groups<std::tuple<T,Ri...>>::type;
		using type = 
			std::conditional_t<
				(1<std::tuple_size<_groups>::value),
				S,
				typename getBranch<std::tuple<Ri...>, tuple_add_t<S,T> >::type
			>;
	};

/*-----------------------------------------------------------------------------------------------
	struct inheritance_tree
*/
template<typename Elements>
struct inheritance_tree;

			template<typename T, size_t I, typename Tuple>
			struct mapper_getBranch{
				using map = std::tuple< typename getBranch<T>::type >;
			};

			template<typename T, size_t I, typename Tuple, typename Branches>
			struct mapper_subTree {
				using map = std::tuple<inheritance_tree<tuple_skip_n<T, std::tuple_size<std::tuple_element_t<I,Branches>>::value>>>;
			};

	template<typename...T>
	struct inheritance_tree<std::tuple<T...>> {
		using groups   = typename inheritance_groups<std::tuple<T...>>::type;
		using branches = tuple_map<groups, mapper_getBranch >;
		using subtrees = tuple_map<groups, mapper_subTree, branches>;
	};
	template<>
	struct inheritance_tree<std::tuple<>> {
		using groups   = std::tuple<>;
		using branches = std::tuple<>;
		using subtrees = std::tuple<>;
	};

/////////////////////////////////////////////////////////////////////////////////////////////////////
		template <typename Tuple, typename WholeSet, typename Index>
		struct __get_branch;

		template <typename...T, typename WholeSet, typename Index>
		struct __get_branch<std::tuple<T...>,WholeSet,Index> {
			static const Index *get() {
				static const Index __branch[sizeof...(T)+1] = {
					sizeof...(T),
					tuple_index<T,WholeSet>...
				};

				return &__branch[1];
			}
		};

		template <typename Tuples, typename WholeSet, typename Index>
		struct __get_branches;

		template <typename...T, typename WholeSet, typename Index>
		struct __get_branches<std::tuple<T...>,WholeSet,Index> {
			static const Index *const *get() {
				static const Index *const __branches[sizeof...(T)] = { __get_branch<T,WholeSet,Index>::get()... };
				return &__branches[0];
			}
		};


	template <typename Index=unsigned short>
	struct Node {
		Index	n;

		const Index	*const *branches;
		const Node	*const *subtrees;
	};

template <typename Tree, typename WholeSet, typename Index=unsigned short>
class build_classTree;

	template<typename, typename WholeSet, typename Index>
	struct __get_subtrees;

	template<typename WholeSet, typename Index>
	struct __get_subtrees<std::tuple<>, WholeSet, Index> {
		static const Node<Index> *const *get() {
			return nullptr;
		}
	};

	template<typename...T, typename WholeSet,typename Index>
	struct __get_subtrees<std::tuple<T...>,WholeSet,Index> {
		static const Node<Index> *const *get() {
			static const Node<Index> *const __nodes[sizeof...(T)] = { build_classTree<T,WholeSet,Index>::get()... };
			return &__nodes[0];
		}
	};

template <typename...T, typename WholeSet, typename Index>
class build_classTree<inheritance_tree<std::tuple<T...>>,WholeSet,Index> {
	using _Tree = inheritance_tree<std::tuple<T...>>;
	enum { _NSUBS = std::tuple_size<typename _Tree::branches>::value };
public:
	static const Node<Index> *get() {
		static const Node<Index> __node{
			_NSUBS,
			__get_branches<typename _Tree::branches, WholeSet, Index>::get(),
			__get_subtrees<typename _Tree::subtrees, WholeSet, Index>::get()
		};
		return &__node;
	}
};

template <typename WholeSet, typename Index>
struct build_classTree<inheritance_tree<std::tuple<>>,WholeSet,Index> {
	static constexpr Node<Index> *get() {
		return nullptr;
	}
};

/////////////////////////////////////////////////////////////////////////////////////
	template<typename To,typename From>
	const void *__dynamic_cast(const void *p) {
		return dynamic_cast<const To*>(static_cast<const From*>(p));
	}

	struct TypeInfoEntry {
		const void *(*cast)(const void*);
		const std::type_info *type_info;
	};

	template<typename Base, typename T, typename List>
	constexpr TypeInfoEntry build_typeInfoEntry() {
		return TypeInfoEntry {
			&__dynamic_cast<T,Base>,
			&typeid(T)
		};
	}


	template<typename WholeSet, typename Base>
	struct __build_tyepInfoList;

	template<typename Base, typename...T>
	struct __build_tyepInfoList<std::tuple<T...>,Base> {
		static constexpr std::array<TypeInfoEntry, sizeof...(T)> build() {
			return std::array<TypeInfoEntry, sizeof...(T)> {
				build_typeInfoEntry<Base,T,std::tuple<T...>>()...
			};
		}
	};

template<typename WholeSet, typename Base>
constexpr std::array<TypeInfoEntry, std::tuple_size<WholeSet>::value>
build_tyepInfoList() {
	return __build_tyepInfoList<WholeSet,Base>::build();
}


/////////////////////////////////////////////////////////////////////////////////
template<typename Bag, typename Index>
bool find_closest_base(const void *x, const Node<Index> *node, Bag &bag, const TypeInfoEntry *entry) {
	assert( node );	//if( ! node ) return false;
	assert( node->branches );
	assert( node->subtrees );

	bool found = false;
	for(int i=0; i<node->n ;++i) {
		const Index *branch = node->branches[i];
		assert( branch );
		assert( 0 < branch[-1] );	//branch length;

		if( entry[branch[0]].cast(x) ) {
			found = true;
			auto subtree = node->subtrees[i];
			if( !subtree || ! find_closest_base(x, subtree, bag, entry) )  {
				int j = branch[-1];	//blen
				while( ! entry[branch[--j]].cast(x) )
					;

				if( end(bag) == find(begin(bag), end(bag), branch[j]) ) {
					bag.push_back(branch[j]);
				}
			}
		}
	}

	return found;
}


/////////////////////////////////////////////////////////////////////////////////////////
	namespace _details {
		template<typename T, typename Elements>
		struct __nearest_bases;

				template<typename SubTree, size_t I, typename List, typename Tree, typename T>
				struct mapper_nearest_from_subtree {
					using branch       = typename std::tuple_element<I,typename Tree::branches>::type;
					using from_subtree = typename __nearest_bases<T, SubTree>::type;
					using map = 
						std::conditional_t<
							!!std::tuple_size<from_subtree>::value,
							from_subtree,
							std::tuple< typename std::tuple_element<std::tuple_size<branch>::value-1, branch>::type >
						>;
				};
		template<typename T, typename Tree>
		struct __nearest_bases {
			using type = tuple_cat_t<
				tuple_map<typename Tree::subtrees, mapper_nearest_from_subtree, Tree,T>
			>;
		};
	}

template<typename T, typename Elements>
struct nearest_bases {
	using type  =
		std::conditional_t<
			0<=tuple_index<T,Elements,false>,
			std::tuple<T>,
			tuple_uniq_t<
				typename _details::__nearest_bases<
					T,
					inheritance_tree<tuple_map<Elements, tuple_filters::is_base_of, T>>
				>::type
			>
		>;
};

template<typename T, typename Elements>
struct nearest_bases_indexed {
	using type = tuple_indices<nearest_bases<T,Elements>,Elements>;
};

}// namespace class_tree////////////////////////////////////////////////////////////////////////////



/*-----------------------------------------------------------------------------------------------
	FX utils
*/
	template<typename FX, typename Args, class=std::__void_t<>>
	struct __FX_exists_callable : std::false_type { };

	template< typename FX, typename...As >
	struct __FX_exists_callable<FX, std::tuple<As...>, std::__void_t<decltype( std::declval<FX>()(std::declval<As>()...) )> >
		: std::true_type { };


	//////////////////////////////////////
	template<typename FX, typename Args, class=std::__void_t<>>
	struct FX_exists_sig : std::false_type { };

	template< typename FX, typename...As >
	struct FX_exists_sig<FX, std::tuple<As...>, std::__void_t<decltype( std::declval<void(FX::*&)(As*...)>() = &FX::operator() )> >
		: std::true_type { };


	//////////////////////////////////////
	template<typename Argv, typename Sig, typename=std::__void_t<>>
	struct is_argv_callable: std::false_type {
		using type = std::tuple<>;
	};

	template<typename...ArgI, typename...ParamI>
	struct is_argv_callable<std::tuple<ArgI...>, std::tuple<ParamI...>, std::__void_t<decltype( std::declval<void(*)(ParamI*...)>()( std::declval<ArgI*>()...) )> > : std::true_type {
		using type = std::tuple< std::tuple<ParamI...> >;
	};


namespace tuple_filters {
	template<typename T, size_t I, typename Tuple, typename FX>
		struct filter_FX_exists: FX_exists_sig<FX, T> {};

	template<typename T, size_t I, typename Tuple, typename Order>
		struct mapper_sort { using map = std::tuple< tuple_sort_t<T, Order> >; };

	template<typename T, size_t I, typename Tuple, typename Param=std::enable_if< (1>tuple_count_v<T,Tuple>) >>
		struct filter_dup : std::integral_constant<int, 1<tuple_count_v<T,Tuple>> {
			static constexpr int error(){ static_assert(0==tuple_count_v<T,Tuple>,"FX_Sigs..........");return 1;}
		};
}




//////////////////////////////////////////////////////////////////////////
//symmetricity support
	template<typename Args, typename Sigs>
	struct get_sigs_callable;

	template<typename Args, typename...SigI>
	struct get_sigs_callable<Args, std::tuple<SigI...> > {
		using type = tuple_cat_t<typename is_argv_callable<Args,SigI>::type...>;
	};



	template<typename Argv, typename Sig>
	struct is_every_sig_callable;

	template<typename Argv, typename...SigI>
	struct is_every_sig_callable<Argv, std::tuple<SigI...>> :
		std::integral_constant<bool, iseq_andAll<iseq<is_argv_callable<Argv,SigI>::value...>>> { 
	};

	template<typename ArgsSet, typename Sig>
	struct is_every_argv_callable;

	template<typename...ArgsI, typename Sig>
	struct is_every_argv_callable<std::tuple<ArgsI...>, Sig> :
		std::integral_constant<bool, iseq_andAll<iseq<is_argv_callable<ArgsI,Sig>::value...>>> { 
	};


/////////////////////////////////////////////////////

	template<typename Argv, size_t I, typename Sigs>
	struct __filter_all_sigs_callable
		: std::integral_constant<bool, is_every_sig_callable<Argv, Sigs>::value> { };

	template<typename Sigs>
	using get_sigs_bestMatch = tuple_map<Sigs, __filter_all_sigs_callable>;

	/////////////////////////////////////////////////////
	template<typename Argv, size_t I, typename Sigs>
		struct _reduce_sigs_filter;

	template<typename Argv, size_t I, typename...SigI>
		struct _reduce_sigs_filter<Argv, I, std::tuple<SigI...>>
			: std::integral_constant<bool, (1>=iseq_sum<iseq<is_argv_callable<SigI,Argv>::value...>>::value)> { };

	template <typename Sigs>
	using reduce_sigs = tuple_map<Sigs, _reduce_sigs_filter>;


///////////////////////////////////////////////////////////////////

	template<typename KT, typename AI, typename TI, typename OP, typename Seq>
	struct __mkey_index_lookup;

	template<typename KT, typename AI, typename TI, typename OP, size_t...I>
	struct __mkey_index_lookup<KT,AI, TI, OP, std::integer_sequence<size_t,I...>> {
#ifdef	__clang__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunsequenced"
#endif
		static int get_indices(const KT &kt,  AI &ai, TI &ti, OP reduce, int sum) {
			std::make_tuple( (sum = reduce(sum, ti[I]=kt[I][ai[I]]))... );
			return sum;
		}
	};
#ifdef	__clang__
#pragma GCC diagnostic pop
#endif
	template<size_t N, typename KT, typename AI, typename TI, typename OP>
	int visitor_mkey_index_lookup(const KT &kt, AI &ai, TI &ti, OP reduce, int sum) {
		return __mkey_index_lookup<KT,AI,TI,OP, std::make_index_sequence<N>>::get_indices(kt, ai, ti, reduce, sum);
	}

///////////////////////////////////////////////////////////////////

	template<typename KT, typename AI, typename TI, typename OP, typename Seq>
	struct __Mkey_index_lookup;
#ifdef	__clang__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunsequenced"
#endif
	template<typename KT, typename AI, typename TI, typename OP, size_t...I>
	struct __Mkey_index_lookup<KT,AI, TI, OP, std::integer_sequence<size_t,I...>> {
		static int get_indices(const KT &kt,  AI &ai, TI &ti, OP reduce, int sum) {
			std::make_tuple( (
				std::tuple_element_t<I,KT>::FULL_DOMAINED
				? 0
				: ( sum = reduce(sum, ti[I]=std::get<I>(kt)[ai[I]]) )
			)... );

			return sum;
		}
	};
#ifdef	__clang__
#pragma GCC diagnostic pop
#endif
	template<size_t N, typename KT, typename AI, typename TI, typename OP>
	int _visitor_mkey_index_lookup(const KT &kt, AI &ai, TI &ti, OP reduce, int sum) {
		return __Mkey_index_lookup<KT,AI,TI,OP, std::make_index_sequence<N>>::get_indices(kt, ai, ti, reduce, sum);
	}




/*------------------------------------------------------------------------------------------------
	assignability
*/

/*
	using Set = tuple<A,B,C,D,X,Y>;
	auto ctable = assignability_table<Set,char>::get_instance();
*/
template<typename T, typename Set, typename ET>
struct __Build_assignability_table_row;


template<typename T,typename...Ti,typename ET>
struct __Build_assignability_table_row<T,std::tuple<Ti...>,ET> {
	enum { SIZE = std::tuple_size<std::tuple<Ti...>>::value };

	static constexpr
	std::array<ET,SIZE> get() {
		return {
			(std::is_assignable<const Ti*&,const T*>::value)...
		};
	}
};

template<typename Set, typename ET>
struct __Build_assignability_table;

template<typename...Ti, typename ET>
struct __Build_assignability_table<std::tuple<Ti...>,ET> {
	enum { SIZE = std::tuple_size<std::tuple<Ti...>>::value };

	static constexpr
	multi_array<ET,SIZE,SIZE> get() {
		return {
			__Build_assignability_table_row<Ti,std::tuple<Ti...>,ET>::get()...
		};
	}
};


template<typename T=unsigned>
struct _Assignability_table {
	int _size;
	T *data() const {
		return reinterpret_cast<T*>(const_cast<_Assignability_table<T>*>(this+1));
	}

	///////////////////////////////////////////////////
	template<typename AT>
	bool is_vector_assignable(int n, const AT *from, const AT *to) const {
		for(int i=0; i<n ;++i) {
			if( ! data()[_size*from[i]+to[i]] )
				return false;
		}
		return true;
	}

	template<typename AT, size_t W>
	bool is_vector_assignable(const std::array<AT,W> &from, const std::array<AT,W> &to) const {
		return is_vector_assignable(W, &from[0], &to[0]);
	}

	template<typename Collection>
	void reduce_sigs(Collection &v, int width) const {
		Collection r;
		copy_if(begin(v),end(v),back_inserter(r),[&v,this,width](auto &x){
			return ! any_of(begin(v),end(v), [&x,this,width](auto &y){
				return &x!=&y && this->is_vector_assignable(width, y, x);
			});
		});
		r.swap(v);
	}

	template<typename AT, size_t W, template<typename,typename>class Collection, template<typename>class Allocator>
	void reduce_sigs(Collection<std::array<AT,W>*,Allocator<std::array<AT,W>*>> &v) const {
		reduce_sigs(*reinterpret_cast<Collection<AT*,Allocator<AT*>>*>(&v), W);
	}
};
/////////////////////////////////////////////////////////////

template<typename Set, typename T=unsigned char>
struct assignability_table {

	template<typename AT>
	bool is_vector_assignable(int n, const AT *from, const AT *to) const {
		return _table.is_vector_assignable(n, from, to);
	}

	template<typename AT, size_t W>
	bool is_vector_assignable(const std::array<AT,W> &from, const std::array<AT,W> &to) const {
		return _table.is_vector_assignable(W, &from[0], &to[0]);
	}

	template<typename Collection>
	void reduce_sigs(Collection &v, int width) const {
		_table.reduce_sigs(v, width);
	}

	template<typename AT, size_t W, template<typename,typename>class Collection, template<typename>class Allocator>
	void reduce_sigs(Collection<std::array<AT,W>*,Allocator<std::array<AT,W>*>> &v) const {
		_table.reduce_sigs(v);
	}

	void print(std::ostream &os = std::cout) const {
		_table.print(os);
	}

	static constexpr assignability_table<Set,T> get_instance() {
		return {
			{std::tuple_size<Set>::value},
			__Build_assignability_table<Set,T>::get()
		};
	}


	enum { SIZE = std::tuple_size<Set>::value };

	_Assignability_table<T>		_table;
	multi_array<T,SIZE,SIZE>	_data;
};




/*--------------------------------------------------------------------------------------------------
	virtual signature
*/


template<typename...>
struct _static{};

template<typename...Ts>
using _domain = std::tuple<Ts...>;


	template<typename T>
	struct is_static_arg : std::integral_constant<bool,!basetype_is_polymorphic<T>::value> {
		using type = std::conditional_t< basetype_is_polymorphic<T>::value, std::tuple<>, std::tuple<T> >;
	};
	template<typename...T>
	struct is_static_arg<_static<T...>>	: std::true_type  { using type = std::tuple<T...>; };

	template<typename T>
	struct remove_static_tag				: std::false_type { using type = std::tuple<T>;		};
	template<typename...T>
	struct remove_static_tag<_static<T...>>	: std::true_type  {  using type = std::tuple<T...>;	};

	///////////////////////////////////////////////////////////////
	template<typename Arg, size_t I, typename Tuple>
	struct filter_static_arg : is_static_arg<Arg> { 
		using map = typename is_static_arg<Arg>::type;
	};

	template<typename Arg, size_t I, typename Tuple>
	struct _mapper_remove_static_tag { using map = typename remove_static_tag<Arg>::type; };

template<typename Args>
using remove_static_tags = tuple_map<Args, _mapper_remove_static_tag>;

template<typename Arg, size_t I, typename Tuple>
struct _mapper_flag_virtual_arg { 
	using map = iseq_n< std::tuple_size< typename remove_static_tag<Arg>::type >::value, !is_static_arg<Arg>::value >;
};



//////////////////////////////////////////////////////////////////////////////////
template<typename _Sig>
struct resolve_virtual_signature {
	using raw_arg_types   = typename resolve_signature<_Sig>::arg_types;

	using static_arg_types   = typename tuple_map2<raw_arg_types, filter_static_arg>::type;
	using virtual_arg_types  = typename tuple_map2<raw_arg_types, filter_static_arg>::complement;

	using return_type = typename resolve_signature<_Sig>::return_type;
	using arg_types   = remove_static_tags<raw_arg_types>;
	using plain_signature   = typename make_signature<return_type, arg_types>::type;
	using virtual_signature = _Sig;


	using virtual_flags   = tuple_mapv<raw_arg_types, _mapper_flag_virtual_arg>;


	enum {
		ARGC  = std::tuple_size<arg_types>::value,
		VARGC = std::tuple_size<virtual_arg_types>::value
	};
	/////////////////////////////////////////////////////////////////


	using index_from_normal = iseq_cat_t< 
		typename iseq_map2<virtual_flags, iseq_filters::filter_nonZero_into_index>::type,
		typename iseq_map2<virtual_flags, iseq_filters::filter_nonZero_into_index>::complement
	>;
	using index_to_normal = iseq_inverse< index_from_normal >;

			template<typename Arg, size_t I, typename Args, typename Indices>
			struct _map_normalized_arg {
				using map = std::tuple<
					std::tuple_element_t<iseq_at<I, Indices>, Args>
				>;
			};
	using normalized_arg_types = tuple_map<arg_types, _map_normalized_arg, index_from_normal>;
};



/////////////////////////////////////////////////////////////////////////////////////////////////////////////
/*
	decorate_virtual_args<VSig, Args>
*/
	template<typename X, size_t I, typename Tuple, typename Virtual_indices, typename ToCastArgs>
	struct _mapper_decorate_virtual_arg {
		using ToCast = std::tuple_element_t<(iseq_at<I,Virtual_indices> < 0 ? 0 : iseq_at<I,Virtual_indices>), ToCastArgs>;
		using map =	std::tuple<
						std::conditional_t< iseq_at<I,Virtual_indices> < 0,		X,
						std::conditional_t< std::is_pointer<X>::value,			std::add_pointer_t<ToCast>,
						std::conditional_t< std::is_lvalue_reference<X>::value,	std::add_lvalue_reference_t<ToCast>,
						std::conditional_t< std::is_rvalue_reference<X>::value,	std::add_rvalue_reference_t<ToCast>,
																				ToCast
						>>>>	
					>;
	};

	template<typename VSig, typename ToCastArgs>
	struct decorate_virtual_args {
		using type = 
			tuple_map<
				typename VSig::arg_types,
				_mapper_decorate_virtual_arg, 
				typename iseq_inc<typename iseq_mul< iseq_partial_sum<typename VSig::virtual_flags>, typename VSig::virtual_flags>::type, -1>::type,
				ToCastArgs
			>;
	};



template<typename T, typename S=T, typename=void>
struct arg_caster {
	static
	std::conditional_t<std::is_rvalue_reference<S>::value,T,T&>
	forward(remove_pointer_or_reference_t<S> &__s) {
		return dynamic_cast<std::conditional_t<std::is_rvalue_reference<S>::value,T,T&>>(__s);
	}

	static
	remove_pointer_or_reference_t<T>*
	forward(remove_pointer_or_reference_t<S> *__s) {
		return dynamic_cast<remove_pointer_or_reference_t<T>*>(__s);
	}
};


template<typename T, typename S>
struct arg_caster<T,S,std::__void_t<decltype( static_cast<remove_pointer_or_reference_t<T>*>((remove_pointer_or_reference_t<S>*)nullptr)  )>> {
	static constexpr
	std::conditional_t<std::is_rvalue_reference<S>::value, T, T&>
	forward(remove_pointer_or_reference_t<S> & __s) noexcept {
		return static_cast<std::conditional_t<std::is_rvalue_reference<S>::value, T, T&>>(__s);
	}

	static constexpr
	remove_pointer_or_reference_t<T>*
	forward(remove_pointer_or_reference_t<S> *__s) noexcept {
		return static_cast<remove_pointer_or_reference_t<T>*>(__s);
	}
};


#ifdef	__clang__
	template<typename S>
	struct arg_caster<S,S> {
		static constexpr
		std::conditional_t<std::is_rvalue_reference<S>::value, S, S &>
		forward(std::remove_reference_t<S> & __s) noexcept {
			return static_cast<std::conditional_t<std::is_rvalue_reference<S>::value,S,S&>>(__s);
		}
	};
#endif



template<typename T> inline
auto &__base_typeid(T &&x) { return typeid(x); }

template<typename T> inline
auto &__base_typeid(T *x) { return typeid(*x); }





/*-------------------------------------------------------------------------------------------------
	__RuntimeError
*/

// exception: multifunction_error_invalid_call
namespace multifunction_error {

	struct invalid_call : std::runtime_error {
		invalid_call(const char*m="multi-function error: function not found or ambiguous call") : runtime_error(m) {}
	};

	struct out_of_domain : invalid_call {
		out_of_domain(const char *m="multi-function error: argument(s) out of domain") : invalid_call(m) {}
	};
}



/////////////////////////////////////////////////////////////////////////////////////
// __runtime_error_function_not_found_or_ambiguous_call()

inline __attribute__((noinline)) __attribute__((noreturn))
void __runtime_error_function_not_found_or_ambiguous_call() {
    throw multifunction_error::invalid_call();
}

template<typename Return>
inline __attribute__((noreturn))
Return __runtime_error_function_not_found_or_ambiguous_call() {
	__runtime_error_function_not_found_or_ambiguous_call();
}


template<typename Return, typename FX, typename...Args>
inline Return __runtime_error_function_not_found_or_ambiguous_call(FX *fx, Args&&...args) {
	return __runtime_error_function_not_found_or_ambiguous_call<Return>();
}


/////////////////////////////////////////////////////////////////////////////////////
// __runtime_error_out_of_domain()
inline __attribute__((noinline)) __attribute__((noreturn))
void __runtime_error_out_of_domain() {
    throw multifunction_error::out_of_domain();
}

template<typename Return>
inline __attribute__((noreturn))
Return __runtime_error_out_of_domain() {
	__runtime_error_out_of_domain();
}


template<typename Return, typename FX, typename...Args>
inline Return __runtime_error_out_of_domain(FX *fx, Args&&...args) {
	return __runtime_error_out_of_domain<Return>();
}


/////////////////////////////////////////////////////////////////////////////////////
template<typename FX, typename Return, typename Args, typename VFlags> 
struct __RuntimeError;

template<typename FX, typename Return, typename...Args, typename VFlags>
struct __RuntimeError<FX, Return, std::tuple<Args...>, VFlags> {

	using Func = Return(*)(FX *fx, Args...args);

	//error: function_not_found_or_ambiguous_call
	static Func get_function_not_found_or_ambiguous_call() {
		return _function_not_found_or_ambiguous_call;
	}
	static Return _function_not_found_or_ambiguous_call(FX *fx, Args...args) {
		return __runtime_error_function_not_found_or_ambiguous_call<Return>(fx, std::forward<Args>(args)...);
	}

	//error: out_of_domain
	static Func get_out_of_domain() {
		return _out_of_domain;
	}
	static Return _out_of_domain(FX *fx, Args...args) {
		return __runtime_error_out_of_domain<Return>(fx, std::forward<Args>(args)...);
	}
};



template<typename T>
struct _Hash;

template<typename T,size_t N>
struct _Hash<std::array<T,N>> : public std::__hash_base<size_t, std::array<T,N>>
{
	size_t operator()(const std::array<T,N> & __a) const noexcept {
		return std::_Hash_impl::hash(__a.data(), __a.size());
	}
};


/*------------------------------------------------------*/
template<typename T>
T *get_base_ptr(T *p) {
	return p;
}

template<typename T>
T *get_base_ptr(T &ref) { 
	return &ref;
}



//////////////////////////////////////////////////////////////////////////////////////////////////////
//ArgDef
	template<typename Domain, size_t I, typename Domains, typename RootType, typename BaseTypes>
	struct _ArgDef_filter_sameBase : std::is_base_of<RootType, typename std::tuple_element<I,BaseTypes>::type> { };

	template<typename RootType, size_t I, typename RootTypeSet, typename Domains, typename BaseTypes>
	struct _ArgDef_map_group_by_root {
		using map =	std::tuple<tuple_uniq_t<
						tuple_cat_t<
							std::tuple<RootType>,
							tuple_catInner<tuple_map<Domains, _ArgDef_filter_sameBase, RootType,BaseTypes>>
						>
					>>;
	};

	template<typename BaseType, size_t I, typename RootTypeSet>
	struct _ArgDef_filter_baseOnly;

	template<typename BaseType, size_t I, typename...TypeI>
	struct _ArgDef_filter_baseOnly<BaseType,I,std::tuple<TypeI...>> : std::__not_< std::__or_< std::__and_<std::__not_<std::is_same<TypeI,BaseType>>, std::is_base_of<TypeI, BaseType>>...  > >
		{ };


	template<typename BaseType, size_t I, typename BaseTypes, typename RootTypeSet>
	struct _ArgDef_map_root {
		using map = tuple_map<RootTypeSet, tuple_filters::is_base_of, BaseType>;
	};

	template <typename _VSig, typename _Domains>
	struct __ArgDef__ {
		using VSig  = _VSig;
		enum { ARGC = VSig::ARGC };

		using BaseTypes		= remove_pointers_or_references< typename VSig::virtual_arg_types >;
		using RootTypeSet	= tuple_map<tuple_uniq_t<BaseTypes>, _ArgDef_filter_baseOnly>;
		using RootDomains	= tuple_map<RootTypeSet, _ArgDef_map_group_by_root, _Domains,BaseTypes>;
		using RootTypes		= tuple_map<BaseTypes, _ArgDef_map_root, RootTypeSet>;


		template<typename D, size_t I, typename __Domains>
		struct map_normalized_domain {
			template<typename Ri, size_t _I, typename RootDomain>
			struct map_helper {
				using Callables = typename get_sigs_callable<std::tuple<Ri>, tuple_map<D, tuple_filters::map_tuple_wrapped>>::type;
				using map = 
					std::conditional_t< !std::tuple_size<Callables>::value,
						std::tuple<>,
						std::tuple_element_t<0,
							tuple_cat_t<
								get_sigs_bestMatch< Callables >,
								std::tuple< std::tuple<Ri> >
							>
						>
					>;
			};
			using map = std::tuple< tuple_uniq_t<tuple_map< 
				std::tuple_element_t< tuple_index<std::tuple_element_t<I,RootTypes>, RootTypeSet>, RootDomains >,
				map_helper
			>>>;
		};
		using Domains = tuple_map<_Domains, map_normalized_domain>;
	};



/////////////////////////////////////////////////////////////////////////////////
namespace __helper__ {

	template<typename Common, typename BaseArgs, typename ToCastArgs,
		typename SigArgs = typename Common::VSig::arg_types,
		typename RealArgs= typename decorate_virtual_args<typename Common::_ArgDef::VSig, ToCastArgs>::type,
		typename ArgISeq = std::make_index_sequence<Common::_ArgDef::ARGC>,
		bool=__FX_exists_callable<typename Common::FX, RealArgs>::value 
	> struct _CallerHelper_MultiDomain;

	//not callable
	template<typename Common, typename...BaseArg, typename ToCastArgs, typename...Sa, typename...Ra, size_t..._AI>
	struct _CallerHelper_MultiDomain<Common,std::tuple<BaseArg...>, ToCastArgs, std::tuple<Sa...>, std::tuple<Ra...>, std::index_sequence<_AI...>, false> {
		static typename Common::Func get_caller() {
			return Common::get_runtime_error_function_not_found_or_ambiguous_call();
		}
	};

	//callable
	template<typename Common, typename...BaseArg, typename...Ta, typename...Sa, typename...Ra, size_t..._AI>
	struct _CallerHelper_MultiDomain<Common,std::tuple<BaseArg...>,std::tuple<Ta...>, std::tuple<Sa...>, std::tuple<Ra...>, std::index_sequence<_AI...>, true> {

		static typename Common::VSig::return_type call(typename Common::FX *fx, Sa...args) {
			return (*fx)( arg_caster<Ra,Sa>::forward(args)... );
		}

		static constexpr
		typename Common::Func get_caller() { return &call; }
	};


	///////////////////////////////////////////////////////////////////////////////

	template<typename Common, typename Args>
	struct caller_generator {
			static constexpr
			typename Common::Func get_caller() {
				return _CallerHelper_MultiDomain<Common,typename Common::_ArgDef::BaseTypes, Args>::get_caller();
			}
	};

	////////////////////////////////////////////////////////////////////////////////
	template<typename Common, typename...DimTs>
	struct _TableBuildHelper;

	template<typename Common, typename...Ts>
	struct _TableBuildHelper<Common, std::tuple<Ts...>> {
		using table_t = std::array<typename Common::Func, sizeof...(Ts)>;

		template <typename Ta=std::tuple<>>
		static constexpr
		table_t get_table() {
			return table_t{ caller_generator<Common, tuple_add_t<Ta,Ts>>::get_caller()...  };
		}
	};

	template<typename Common, typename...Ts, typename...Ds>
	struct _TableBuildHelper<Common, std::tuple<Ts...>,Ds...> {
		using Prev = _TableBuildHelper<Common,Ds...>;
		using table_t = std::array< typename Prev::table_t, std::tuple_size<std::tuple<Ts...>>::value >;

		template <typename Ta = std::tuple<>>
		static constexpr
		table_t get_table() {
			return	table_t{ Prev::template get_table<tuple_add_t<Ta,Ts>>()...  };
		}
	};


	/////////////////////////////////////////////////////////////////////////////////////////////
	//struct Common
	template<typename _FX, typename __ArgDef>
	struct Common {
		using FX = _FX;
		using _ArgDef = __ArgDef;
		using VSig = typename __ArgDef::VSig;

		using BaseTypes = typename _ArgDef::BaseTypes;
		using Func = typename make_signature<typename VSig::return_type, tuple_cat_t<std::tuple<FX*>, typename VSig::arg_types>>::type*;


		//////////////////////////////////////////////////////////////////////////////////////
		using __RuntimeError = __RuntimeError<FX,typename VSig::return_type, typename VSig::arg_types, typename VSig::virtual_flags>; 

		static Func get_runtime_error_function_not_found_or_ambiguous_call() {
			return __RuntimeError::get_function_not_found_or_ambiguous_call();
		}

		static Func get_runtime_error_out_of_domain() {
			return __RuntimeError::get_out_of_domain();
		}
	};


	//////////////////////////////////////////////////////////////////////////////////
	template<typename FX, typename __ArgDef>
	struct __TableBuilder;

	template<typename FX, typename _VSig, typename...Ds>
	struct __TableBuilder<FX, __ArgDef__<_VSig,std::tuple<Ds...>>> {
		using _ArgDef = __ArgDef__<_VSig,std::tuple<Ds...>>;
		using _Helper = _TableBuildHelper< Common<FX,_ArgDef>, Ds...>;
		using table_t = typename _Helper::table_t;
		//////////////////////////////////////////////////////////
		using _Common = Common<FX,_ArgDef>;
		using Func = typename _Common::Func;

		static
		constexpr table_t get_table() {
			return _Helper::get_table();
		}
	};

	///////////////////////////////////////////////////////////////////////////////////////////
	template<typename R, typename D, typename ET=int, bool=std::is_same<R,D>::value>
	struct _KeyTableRow;

	template<typename...Rs, typename D, typename ET>
	struct _KeyTableRow<std::tuple<Rs...>, D, ET,false> {
		enum { FULL_DOMAINED = false };

		ET operator[](int i) const { return data[i]; }

		static constexpr _KeyTableRow get() {
			struct Dummy;
			return _KeyTableRow {
				tuple_index<
					std::tuple_element_t<0,
						std::tuple_element_t<0,
							tuple_cat_t< get_sigs_bestMatch< typename get_sigs_callable<std::tuple<Rs>, tuple_map<D, tuple_filters::map_tuple_wrapped>>::type >, std::tuple<std::tuple<Dummy>> >
						>
					>,
					D,
					false
				>...
			};

		}

		std::array<ET, sizeof...(Rs)>	data;
	};



	template<typename...Rs, typename D, typename ET>
	struct _KeyTableRow<std::tuple<Rs...>,D, ET,true> {
		enum { FULL_DOMAINED = true };
		constexpr ET operator[](int i) const { return i; }

		static constexpr _KeyTableRow get() {
			return {};
		}

		std::array<ET,0>	data;
	};


	////////////////////////////////////////////////////////////

	template<typename ArgDef>
	struct _KeyTableBuilder;

	template<typename _VSig, typename...Ds>
	struct _KeyTableBuilder<__ArgDef__<_VSig,std::tuple<Ds...>>> {
		using _ArgDef = __ArgDef__<_VSig,std::tuple<Ds...>>;

		static constexpr auto get_table() {
			return std::make_tuple(
				_KeyTableRow<
					std::tuple_element_t<
						tuple_index<
							std::tuple_element_t<tuple_index<Ds,std::tuple<Ds...>>,typename _ArgDef::RootTypes>,
							typename _ArgDef::RootTypeSet
						>,
						typename _ArgDef::RootDomains
					>,
					Ds
				>::get()...
			);
		}

		using table_t = decltype(get_table());
	};

}//namespace __helper__



///////////////////////////////////////////////////////////////////
template<typename _Sig, 
	typename _Args=typename resolve_virtual_signature<_Sig>::arg_types, 
	typename _ArgSeq =std::make_index_sequence<std::tuple_size<_Args>::value>, 
	typename _VArgSeq=std::make_index_sequence<std::tuple_size< typename resolve_virtual_signature<_Sig>::virtual_arg_types >::value>,
	typename _NormalizedArgs = typename resolve_virtual_signature<_Sig>::normalized_arg_types
	>
struct __multifunc;


template<typename _Sig, typename..._Args, size_t..._AI, size_t..._VI, typename..._NArgs>
struct __multifunc<_Sig, std::tuple<_Args...>, std::index_sequence<_AI...>, std::index_sequence<_VI...>, std::tuple<_NArgs...>> {
protected:
	enum { ARGC = sizeof...(_Args) };
	enum { VARGC = sizeof...(_VI) };

	using VSig = resolve_virtual_signature<_Sig>;
	using return_type = typename VSig::return_type;
	using arg_types   = typename VSig::arg_types;
	using virtual_arg_types = typename VSig::virtual_arg_types;
	using static_arg_types  = typename VSig::static_arg_types;
	using index_from_normal = typename VSig::index_from_normal;

	virtual return_type _call(_NArgs...argi) = 0;

public:
	return_type call(_Args...argi) {
		typename VSig::arg_types	__args{std::forward<_Args>(argi)...};	//will be optimized away by the compiler
		return _call( std::forward<_NArgs>(std::get<iseq_at<_AI, index_from_normal>>(__args))... );
	}
};



////////////////////////////////////////////////////////////////////////
template<typename FX,
		template <typename...> class Map = std::unordered_map,
		typename Args=typename resolve_virtual_signature<typename FX::type>::arg_types, 
		typename _ArgSeq=std::make_index_sequence<std::tuple_size<Args>::value>,
		typename _VArgSeq=std::make_index_sequence<std::tuple_size< typename resolve_virtual_signature<typename FX::type>::virtual_arg_types >::value>,
		typename _NormalizedArgs = typename resolve_virtual_signature<typename FX::type>::normalized_arg_types
		>
struct multifunc;


template<typename FX, template <typename...> class Map, typename..._Args, size_t..._AI, size_t..._VI, typename..._NArgs>
struct multifunc<FX,Map,std::tuple<_Args...>, std::index_sequence<_AI...>, std::index_sequence<_VI...>, std::tuple<_NArgs...>>
	: __multifunc<typename FX::type>, FX
{
protected:
	using Super =  __multifunc<typename FX::type>;
	using typename Super::return_type;
	using typename Super::VSig;
	using typename Super::virtual_arg_types;
	using typename Super::static_arg_types;

	enum { ARGC  = Super::ARGC };
	enum { VARGC = Super::VARGC };
	using _ArgDef = __ArgDef__<VSig, typename __ArgDef__<VSig, typename FX::Domains>::Domains>;
	using TableBuilder = __helper__::__TableBuilder<FX,_ArgDef>;
	using table_t = typename TableBuilder::table_t;

	using KeyTableBuilder = __helper__::_KeyTableBuilder<_ArgDef>;
	using key_table_t = typename KeyTableBuilder::table_t;


	using Domains		= typename _ArgDef::Domains;
	using BaseTypes		= typename _ArgDef::BaseTypes;
	using RootTypes		= typename _ArgDef::RootTypes;
	using RootTypeSet	= typename _ArgDef::RootTypeSet;
	using RootDomains	= typename _ArgDef::RootDomains;	//args-domains
	enum { ROOTC = std::tuple_size<RootTypeSet>::value };


	///////////////////////////////////////////////////////////////////
	template<size_t I>
	struct _root_index_of
		: std::integral_constant<int, tuple_index<std::tuple_element_t<I,RootTypes>, RootTypeSet>> { };

	template<size_t I>
	static constexpr int root_index_of = _root_index_of<I>::value;

	using Func = typename make_signature<typename VSig::return_type, tuple_cat_t<std::tuple<FX*>, typename VSig::arg_types>>::type*;
	using typemap_t = Map<size_t, int>;


	struct MappingInfo {
		const table_t			*table;
		typemap_t				typemap;
		std::vector<std::vector<int>>		_multi_bases;
		Map<std::array<int,VARGC>, Func,_Hash<std::array<int,VARGC>>>	multi_map;

		std::vector<int>	multi_bases;
		std::vector<int>	multi_bases_storage;
	};
	using type_index_t = unsigned short;

	using typename Super::index_from_normal;
	using normalized_arg_types = typename VSig::normalized_arg_types;

	using FX::FX;

	static const table_t &get_table() {
		static const table_t __table = TableBuilder().get_table();
		return __table;
	}


public:
	virtual return_type _call(_NArgs...argi)
	{
		MappingInfo &mappingInfo = get_mappingInfo();

		std::tuple<_NArgs...> __args{std::forward<_NArgs>(argi)...};

		int sum = 0, tmp;
		std::array<int, VARGC> ai{ (tmp=mappingInfo.typemap[__base_typeid(std::get<_VI>(__args)).hash_code()]-1, sum|=(unsigned)tmp,tmp)... };


		if( 0 <= sum ) {
			sum = _visitor_mkey_index_lookup<VARGC>(get_key_table(), ai, ai, std::bit_or<int>(), 0);
			if( 0 <= sum ) {
				auto func = multi_array_getAt(*mappingInfo.table, ai);
				return func(this, std::forward<_Args>(std::get< iseq_at<_AI, typename VSig::index_to_normal> >(__args))...);
			}

			if( sum==-1 ) {
				return TableBuilder::_Common::get_runtime_error_out_of_domain()
						(this, std::forward<_Args>(std::get< iseq_at<_AI, typename VSig::index_to_normal> >(__args))...);
			}
		}

		if( sum < -1 ) {
			auto func = mappingInfo.multi_map[ai];
			if( func ) {
				return func(this, std::forward<_Args>(std::get< iseq_at<_AI, typename VSig::index_to_normal> >(__args))...);
			}
			assert(!"function not found in cache");
		}

		return handle_invalid_argtypes(ai, std::forward<_NArgs>(argi)...);

	}//end-of _call()//////////////////////////////////////////////////////////////

protected:
	return_type 
	handle_invalid_argtypes( std::array<int,VARGC> &ai, _NArgs...argi)
	{
		auto classTree = get_classTree();
		const auto &typeInfo = get_typeInfo();
		MappingInfo &mappingInfo = get_mappingInfo();


		std::tuple<_NArgs...> __args{std::forward<_NArgs>(argi)...};
		std::array<void*, VARGC> vargv{ get_base_ptr(std::get<_VI>(__args))... };

		std::array<int, VARGC> nBases;	nBases.fill(1);
		std::array<int, VARGC> pBases;
		std::vector<int> base_store;

		std::vector<int> bases;
		int sum = 0;
		for(int i=0; i<VARGC ;++i) {
			pBases[i] = base_store.size();
			size_t hash_code;

			struct Dummy	{ virtual ~Dummy()=0; };
			if( ai[i]==-1 ) {
				hash_code = typeid(*static_cast<Dummy*>(vargv[i])).hash_code();
				ai[i] = mappingInfo.typemap[hash_code]-1;
			}

			if( ai[i]==-1 ) {
				bases.clear();
				class_tree::find_closest_base(vargv[i], classTree[i], bases, typeInfo[i].data);
				if( bases.size()<=1 ) {
					ai[i] = (bases.size() ? bases[0] : 0);
					mappingInfo.typemap[hash_code] = ai[i]+1;
				}
				else {
					mappingInfo._multi_bases.push_back( bases );
					mappingInfo.typemap[hash_code] = ai[i] = mappingInfo._multi_bases.size() | ~(unsigned(-1)>>1);
					--ai[i];
				}
			}

			sum |= ai[i];

			if ( ai[i] >= 0 ) {
				nBases[i] = 1;
				base_store.push_back(ai[i]);
			}
			else {
				auto &_bases = mappingInfo._multi_bases[ai[i] & (unsigned(-1)>>1)];
				nBases[i] = _bases.size();
				std::copy( begin(_bases), end(_bases), back_inserter(base_store));
			}
		}//end-for

		assert( sum !=-1 );
		if( 0 <= sum )	{
			return _call(std::forward<_NArgs>(argi)...);	//re-visit
		}

		//case: sum < 0 //////////////////////////////////////////////////////////////////

		auto vtable = *mappingInfo.table;

		std::vector<std::array<int,VARGC>> sigs;
		std::vector<typename TableBuilder::Func> funcs;
		foreach_cartesian<VARGC>(
			nBases,
			[&vtable,&funcs,&base_store,&pBases,&sigs](std::array<int,VARGC> &key){
				std::array<int,VARGC> index;
				for(int i=0; i<VARGC ;++i)
					index[i] = base_store[pBases[i] + key[i]];

				int sum = _visitor_mkey_index_lookup<VARGC>(get_key_table(), index, index, std::bit_or<int>(), 0);
				assert( 0 <= sum );

				auto func = multi_array_getAt(vtable, index);
				if(	func != TableBuilder::_Common::get_runtime_error_function_not_found_or_ambiguous_call() ) {
					funcs.push_back(func);
					sigs.push_back(index);
				}
			} 
		);


		typename TableBuilder::Func func = nullptr;
		if ( 1<=funcs.size() ) {
			std::vector<std::array<int,VARGC>*> best_sigs;
			for(auto &x : sigs )
				best_sigs.push_back(&x);

			get_assignability_table().reduce_sigs(best_sigs);

			//func = funcs[0];
			func = multi_array_getAt(vtable, *best_sigs[0]);
		}
		else {
			func = TableBuilder::_Common::get_runtime_error_function_not_found_or_ambiguous_call();
		}
		assert( func );

		mappingInfo.multi_map[ai] = func;

		return func(this, std::forward<_Args>(std::get< iseq_at<_AI, typename VSig::index_to_normal> >(__args))...);
		//return _call(std::forward<_NArgs>(argi)...);	//re-visit//XXX
	}

	////////////////////////////////////////////////////////////////////////////////////

	static const key_table_t &get_key_table() {

		static const key_table_t __key_table = KeyTableBuilder().get_table();

		return __key_table;
	}


	static MappingInfo &build_mappingInfo() {
		static MappingInfo __info = {&get_table()};
		build_map( __info.typemap );
		return __info;
	}

	static MappingInfo &get_mappingInfo() {
		static MappingInfo &__info = build_mappingInfo();
		return __info;
	}

	static void build_map(typemap_t &__map) {

		auto &typeInfo = get_root_typeInfo();

		for(int j=0; j<ROOTC ;++j) {
			for(int i=0; i < typeInfo[j].size ;++i) {
				__map[typeInfo[j].data[i].type_info->hash_code()] = i+1;
			}
		}
	}


	////////////////////////////////////////////////////////////////////////////////////
	template<size_t...I>
	static const auto &_get_root_typeInfo(std::index_sequence<I...> ) {
		static const auto typeinfo_tuple =
			std::make_tuple(
				class_tree::build_tyepInfoList<
					std::tuple_element_t<I,RootDomains>,
					std::tuple_element_t<I,RootTypeSet>
				>()...
			);

		static const std::array<sized_pdata<const class_tree::TypeInfoEntry>,ROOTC> 
			__infos{ sized_pdata<const class_tree::TypeInfoEntry>(std::get<I>(typeinfo_tuple).size(), std::get<I>(typeinfo_tuple).data())... };

		return __infos;
	}

	static const auto &get_root_typeInfo() {
		return _get_root_typeInfo(std::make_index_sequence<ROOTC>());
	}

	template<size_t...I>
	static const auto &build_typeInfo(std::index_sequence<I...>) {
		auto &root_typeInfo = get_root_typeInfo();

		static const std::array<sized_pdata<const class_tree::TypeInfoEntry>,VARGC> 
			__infos{ root_typeInfo[root_index_of<I>]... };

		return __infos;
	}

	static const auto &get_typeInfo() {
		static const auto &__infos = build_typeInfo(std::make_index_sequence<VARGC>());
		return __infos;
	}

	static const auto &get_assignability_table() {
		static const auto table = assignability_table<RootDomains>::get_instance();
		return table;
	}


	////////////////////////////////////////////////////////////////////////////////////
	static const std::array<const class_tree::Node<type_index_t> *, VARGC>
	&get_classTree() {
		using namespace vane::class_tree;

		static std::array<const Node<type_index_t> *, VARGC> __classTrees = {
			build_classTree<
				inheritance_tree<tuple_map<std::tuple_element_t<root_index_of<_VI>,RootDomains>, tuple_filters::is_not_same, std::tuple_element_t<root_index_of<_VI>,RootTypeSet>>>,
				std::tuple_element_t<root_index_of<_VI>,RootDomains>,
				type_index_t
			>::get()...
		};

		return __classTrees;
	}
};


}//namespace vane//////////////////////////////////////////////////////////////////////////////////////////////////////////////
#endif	//___VANE_H_20170719
// vim: ts=4

