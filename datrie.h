#ifndef DATRIE_H_DFD2ADCA_97C3_4373_8007_DB6687FECA95
#define DATRIE_H_DFD2ADCA_97C3_4373_8007_DB6687FECA95
/**
 * \file datrie.h
 *
 * \brief Public header file for the 'datrie' module
 *
 *
 * This is  the header file for  datrie, a C module  providing a generic
 * implementation of a double-array trie implementation (see \ref datrie
 * "below").
 *
 *
 */

/**
 * \page datrie Double-array trie
 *
 * \section datrie_intro Introduction
 *
 * A trie (AKA a prefix tree)  is an ordered tree data structure that is
 * used to store associative arrays.   The descendents of any given node
 * share the string represented by that node as a common prefix to their
 * associated strings.   This makes for  both very fast text  lookup and
 * convenient prefix matching.  Values  are normally not associated with
 * every  node, only with  leaves and  some inner  nodes that  happen to
 * correspond to keys of interest.
 *
 * Module datrie provides  a trie implementation in ANSI  C based on two
 * arrays plus string memory as  laid out by \ref datrie_ref_1 "Aoe" (on
 * which more below).
 *
 *
 * \section datrie_impl Discussion
 *
 * \subsection datrie_naive Naive Approaches
 *
 * The double-array implemetation is suggested  by viewing the trie as a
 * deterministic finite-state  automaton, or DFA (AKA an  FSM, or finite
 * state  machine).  Each  node  in the  trie  maps to  a  state in  the
 * machine,  and each character  in a  search string  is an  input event
 * causing a state transition (see, e.g.  \ref datrie_ref_2 "Knuth").
 *
 * As an  example, consider  the set  of strings K  = {  baby, bachelor,
 * back,  badge,  badger,  badness,  bcs  }.   This  would  produce  the
 * following trie:
 *
 \code

                       s1
                       | b
                       s3 - c - s9 - s - s34 "bcs"
                       | a
              s4  - b- s7 - d - s6  -------- n
               |y      | c           |g      |
              s21      s5 -- k       s8     s15
            "baby"     | h   |       |e      |e
                       s10   s12     s11    s30
                       | e "back" "badge"    |s
                       s23           |r     s31
                       | l          s19      |s
                       s24        "badger"  s32
                       | o                "badness"
                       s25
                       | r
                       s28
                  "bachelor"
 \endcode
 *
 * We could encode this as a DFA with the following transition table:
 *
 \code
  State a b c d e f g h i j k l m n o p q r s t u v w x y z
  s1      s3
  s3    s7  s9
  s4                                                    s21
  s5                  s10   s12
  s6                              s15
  s7      s4s5s6
  s8            s11
  s9                                        s34
  s10           s23
  s11                                     s19
  s12
  s15           s30
  s19
  s23                        s24
  s24                               s25
  s25                                     s28
  s30                                       s31
  s31                                       s32
  s32
  s34
 \endcode
 *
 * As you  can see, most of  the transition table is  wasted space (only
 * 3.7%  of  the  space is  being  used,  and  we're only  allowing  the
 * characters  a-z   as  input;  if  we  allowed   all  printable  ASCII
 * characters,  the percentage of  the table  in use  would drop  to one
 * percent!).  This  suggests looking for more  space-efficient means of
 * storing the transtion table.
 *
 *
 * \subsection datrie_da Double Array Trie Implementation
 *
 * This  module is  just a  C implementation  of that  laid out  in \ref
 * datrie_ref_1 "Aoe".   It compresses  the transition table  above into
 * two arrays  which he names BASE  & CHECK, together with  a buffer for
 * storing text named TAIL.
 *
 * More formally,  the tree for a set  of search strings (or  keys) K is
 * formally defined by a 5-tuple (S, I, g, s(1), A) where:
 *
 * - S is a finite set of nodes.
 *
 * - I is a finite set of input symbols.
 *
 * - g is a function from S X I to S U {fail} called a goto function.
 *
 * - s(1) is the initial node, or the root in S.
 *
 * - A is a finite set of accepting nodes.
 *
 * For s(r) and s(t) in S, g( s(r), a ) = s(t) IFF the double-array for
 * K holds:
 *
 \code

  t = BASE[ r ] + a and CHECK[ t ] = r

 \endcode
 *
 * or pictorially:
 *
 \code

      BASE        CHECK

  r  BASE[r] -
              |
              a
              |
  t           v    r

 \endcode
 *
 * Note that  each node  s(r) in S  corresponds to  an index r,  or node
 * number, of the  double-array and that each input  symbol a is treated
 * as a numeric value.
 *
 * Continuing the  example started \ref datrie_naive  "above", Aoe would
 * encode the search trie into two arrays (plus string memory) like so:
 *
 * \anchor datrie_eg_1
 *
 \code
         BASE   CHECK

      1  1      20
      2  0       0
      3  6       1
      4 -17      7
      5  2       7
      6  1       7
      7  2       3
      8  6       6
      9 -10      3
     10 -1       5
     11  1       8
     12  0       0
     13 -20      5
     14  0       0
     15 -24      6
     16  0       0
     17  0       0
     18  0       0
     19  22     11
     20  13     11

     TAIL
                       1                   2
     1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8
     e l o r # $ ? ? ? s # $ $ ? ? ? y # $ # $ # $ e s s # #

 \endcode
 *
 * Notes:
 *
 * - We've encoded a as 1, b as 2, and so on.
 *
 * - The '#' character in TAIL denotes the string terminator
 *
 * - The '$' in TAIL denotes unused storage
 *
 * To  compare,  the naive  implementation  took  884  bytes of  memory,
 * whereas Aoe's  requires only  2 *  20 + 28  = 68  to encode  the same
 * information (a thirteen-fold improvement)!
 *
 *
 * \subsubsection datrie_defns Defintions
 *
 * The following nodes are defined on the tree:
 *
 * <ol>
 *
 * <li> For key "xay" in K, we define node s(r) such that g( s(1), xa )
 * = s(r) as a \em separate \em node if a is a sufficient symbol for
 * distinguishing the key xay from all others in K.
 *
 * <li>Each node in a path from the initial node to a separate node is
 * called a multinode.
 *
 * <li>Each node in a path from a separate node to the accepting node
 * is called a single-node.
 *
 * </ol>
 *
 * Let S(P), S(M) and S(1) be sets of separate nodes, multinodes, and
 * single-nodes, respectively. Note that S(P) = S(M) intersect S(1).
 *
 * A string x  such that g( s(r), x  ) = s(t) for s(r) in  S(P), and for
 * s(t) in  A is called a  \em singlestring for the  separate node s(r),
 * and is denoted by STR[ s(r) ].
 *
 *
 * \subsubsection datrie_retrieve Algorithm 1: Retrieval
 *
 * So,  how do  we use  this structure  to lookup  text?   The retrieval
 * algorithm is as follows:
 *
 \code

  input:  A string x = a(l)a(2)...a(n)
  output: If x E K, then the output is TRUE, otherwise FALSE.
  method:

   begin

     Initialize the current node number r and the current input position
     h to 1 and 0, respectively;

     repeat

       h := h + 1;
       Set the next node number t to BASE[ r ] + a(h);
       if t exceeds DA-SIZE or CHECK[ t ] is unequal to r then
       return FALSE (1)
       else set r to t

     until BASE[ r ] < 0;

     if h reaches the last position n then return TRUE
     else set s-temp to the single-string FETCHSTR( - BASE[ r ] );

     if STRCMP( a(h+1)...a(n), s-temp ) == 0 then return TRUE
     else return FALSE (2)

   end

 \endcode
 *
 * Notes:
 *
 * 1. At this point, we've traversed the tree through a sequence of
 * multi-nodes & found the longest prefix of x in K; namely
 * a(1)...a(h-1).
 *
 * 2. At this point, we've walked the tree down to a single-node, but x
 * doesn't match the string corresponding to that node.
 *
 * This algorithm requires the following helper functions & variables:
 *
 * - DA-SIZE: The size of the double-array
 *
 * - FETCHSTR( p ): Return a string TAIL[p]TAIL[p+l]...TAIL[p+k] such
 *   that TAIL[p + k] == #
 *
 * - STRCMP(x, y): Return -1 if a string x is equal to string y,
 *   otherwise return the length of the longest prefix of x and y.
 *
 * Let's work  out the retrieval of  the key "badness"  from the example
 * trie described  \ref datrie_eg_1 "above".   We initialize r :=  1 and
 * h := 0, then:
 *
 \code

   h := h + 1 = 1;
   t := BASE[ r ] + a(h) = BASE[ 1 ] + 'b' = 1 + 2 = 3;
   t <= DA-SIZE && CHECK[ 3 ] = 1, so r := 3;
   BASE[ 3 ] > 0, so repeat

   h := h + 1 = 2;
   t := BASE[ r ] + a(h) = BASE[ 3 ] + 'a' = 6 + 1 = 7;
   t <= DA-SIZE && CHECK[ 7 ] = 3, so r := 7;
   BASE[ 7 ] > 0, so repeat

   h := h + 1 = 3;
   t := BASE[ r ] + a(h) = BASE[ 7 ] + 'd' = 2 + 4 = 6;
   t <= DA-SIZE && CHECK[ 6 ] = 7, so r := 6;
   BASE[ 6 ] > 0, so repeat

   h := h + 1 = 4;
   t := BASE[ r ] + a(h) = BASE[ 6 ] + 'n' = 1 + 14 = 15;
   t <= DA-SIZE && CHECK[ 15 ] = 6, so r := 15;
   BASE[ 15 ] = -24 < 0, so break

   stemp = FETCHSTR( 24 ) = "ess", "badn" + "ess" = "badness", so
   return TRUE

 \endcode
 *
 * Think  of the  indicies  into the  double-array  as corresponding  to
 * states.  For a state r, BASE[ r ] gives the base offset for computing
 * transitions to  states reachable from r  (of course, the  set of such
 * states is likely  to be small compared to  the input alphabet!)  Once
 * we've reached  a node which only has  one key beneath it,  we ram the
 * remainder of the string into TAIL.
 *
 *
 * \subsubsection datrie_insert Algorithm 2: Insertion
 *
 * The insertion algorithm is suggested  by the two points of failure in
 * the insertion algorithm.
 *
 \code

  input:  A string x = a(l)a(2)...a(n)
  output: If x E K, then the output is TRUE, otherwise FALSE.
  method:

   begin

     Initialize the current node number r and the current input position
     h to 1 and 0, respectively;

     repeat

       h := h + 1;
       Set the next node number t to BASE[ r ] + a(h);
       if f exceeds DA-SIZE or CHECK[ t ] is unequal to r then
         AINSERT(r, a(h),a(h+1)...a(n))
         return FALSE
       else set r to t

     until BASE[ r ] < 0;

     if h reaches the last position n then return TRUE
     else set s-temp to the single-string FETCHSTR( - BASE[ r ] );

     if STRCMP( a(h+1)...a(n), s-temp ) == 0 then return TRUE
     else
       BINSERT( r, a(h+1)...a(h+k)a(h+k+1)...a(n)b(1)...b(m) )
       return FALSE

   end

 \endcode
 *
 * The  new procedure  AINSERT corresponds  to point  1 in  algorithm 1.
 * Here, a(1)...a(h-1) has led us from s(1) to some state s(r).  At this
 * point, there is no transition on a(h) out of s(r).
 *
 * AINSERT appends an arc g( s(r), a(h) ) = s(t) to the double-array and
 * stores the singlestring STR[ s(t) ] = a(h+1)...a(n) in TAIL.
 *
 * Pictorially, the situation looks like this:
 *
 \code
        a(1)...a(h-1)
  s(1) ---------------> s(r)
                         |
                        a(h) <---- Added by AINSERT
                         |                |
                         v                v
                        s(t) STR[ s(t) ] = a(h+1)...a(n)

 \endcode
 *
 * BINSERT corresponds to point 2  in algorithm 1.  At this point, we've
 * reached the  single-node s(r), there there must  be different symbols
 * b(1)    &    a(h+k+1)    for    s-temp    =    STR[    s(r)    ]    =
 * a(h+1)a(h+2)...a(h+k)b(1)...b(m)  and   the  remaining  input  string
 * a(h+1)...a(h+k)a(h+k+1)...a(n).  In other words:
 *
 \code

  Input:    a(1)a(2)...a(h)a(h+1)...a(h+k)a(h+k+1)...a(n)
  STR[s(r)]:               a(h+1)...a(h+k)b(1) ...   b(m)
                           <--  same  -->  ^
                                           |
               first different character---
 \endcode
 *
 * Pictorially:
 *
 \code
        a(1)...a(h)
  s(1) ------------> s(r)

        STR[ s(r) ] = a(h+1)...a(h+k)b(1)...b(m)

 \endcode
 *
 * BINSERT appends arcs g( s(r), a(h+1)...a(h+k) ) = s(t),
 * g( s(t), b(1) ) = s(t'), and g( s(t), a(h+k+1) ) = s(t''),
 * and stores STR[ s(t') ] = b(1)...b(m) &
 * STR[ s(t'') ] = a(h+k+1)...a(n) in TAIL:
 *
 \code
        a(1)...a(h)      a(h+1)...a(h+k)       b(1)
  s(1) ------------> s(r) --------------> s(t) ----> s(t')
                                           |   STR[ s(t') ] = b(2)...b(m)
                                        a(h+k+1)
                                           |
                                            --> s(t'')
                            STR[ s(t'') ] = a(h+k+2)...a(n)
 \endcode
 *
 *
 *
 * \datrie_eg_3 Consider inserting "bcsd":
 *
 \code

  r := 1, h := 0
  h := h + 1 = 1
  t = BASE[ r ] + a(1) = BASE[ 1 ] + 'b' = 1 + 2 = 3
  CHECK[ 3 ] = 1 so r := t = 3
  h := h + 1 = 2
  t = BASE[ r ] + a(2) = BASE[ 3 ] + 'c' = 6 + 3 = 9
  BASE[ 9 ] = -10 so break

  set s-temp to the single-string FETCHSTR( - BASE[ r ] ) = "s"
  STRCMP( a(h+1)...a(n), s-temp ) = STRCMP( "sd", "s" ) = 1 != 0,
  so we're going to call BINSERT( 3, "sd" )

 \endcode
 *
 *
 * \section datrie_use Using This Module
 *
 * This code is intended to be  usable on any platform, in both kernel &
 * user  modes.   As   such,  I've  tried  to  factor   out  any  system
 * dependecies,  even C  runtime library  calls (which  generally aren't
 * available in Kernel  mode on Windows, for instance).   Client code is
 * expected to provide  the following #define's \em before  this file is
 * included:
 *
 * - DATRIE_MALLOC(  ctx, cb  ): memory  allocator, defaults  to malloc.
 *   Assumed  to accept  a tDaTrieContext  and  the number  of bytes  to
 *   allocate, as a size_t.  It shall return a void*, and shall indicate
 *   failure by returning zero.
 *
 * - DATRIE_FREE(  ctx,  p ):  memory  de-allocator,  defaults to  free.
 *   Takes a tDaTrieContext and a pointer to void.  Returns void.
 *
 * - DATRIE_REALLOC( ctx, p, cb ): memory re-allocator; defaults to
 *   realloc.  Assumed to have a signature of void* ( * )(
 *   tDaTrieContext, void*, size_t ).
 *
 * - DATRIE_MEMCPY(  ctx, dest, src,  cb ):  memory copier,  defaults to
 *   memcpy.  Assumed to have a signature of void ( * )( tDaTrieContext,
 *   void*, const void*, size_t).
 *
 * - DATRIE_MEMSET(  ctx, dest, val,  cb ):  memory setter,  defaults to
 *   memset.  Assumed to have a signature of void ( * )( tDaTrieContext,
 *   void*, char, size_t).
 *
 * - DATRIE_STRLEN( ctx, s ): string length; defaults to strlen.
 *   Assumed to have a signature of size_t ( * )( tDaTrieContext, const
 *   char * )
 *
 * - DATRIE_STRCPY( ctx, dest, src ):  string copy; defaults to memmove,
 *   since   this   implementation   will  try   to   copy   overlapping
 *   strings.   Assumed  to   have  a   signature  of   char*  (   *  )(
 *   tDaTrieContext, char *, const char * )
 *
 * - DATRIE_ASSERT( cond ): Assertion macro-- defaults to assert.
 *
 * This arrangement allows callers  to provide custom memory management,
 * as  well as making  this code  runnable as  is in  environments where
 * malloc isn't available (such as Windows Kernel-mode drivers).
 *
 * If DATRIE_INCLUDE_DUMP_CODE is #defined, the following macros are
 * also expected to be provided by the caller.  They all default to
 * printf.
 *
 * - DATRIE_LOG0( ctx, msg )
 *
 * - DATRIE_LOG1( ctx, fmt, p0 )
 *
 * - DATRIE_LOG2( ctx, fmt, p0, p1 )
 *
 * - DATRIE_LOG3( ctx, fmt, p0, p1, p2 )
 *
 * This allows the caller to control  whether output goes to stdout or a
 * file,  or  to  run  in  an  environment where  printf  might  not  be
 * available.
 *
 * The \c  ctx parameter above  is intended to  be used in case  of some
 * sort of context parameter, and is ignored by the defaults.
 *
 *
 * \subsection datrie_use_eg Example
 *
 * To continue the example \ref datrie_eg_1 "above", here's how we might
 * use datrie:
 *
 \code

  #include "datrie.h"

  int main( )
  {
      hDaTrie       hTrie;
      tDaTrieStatus status;
      size_t        n;
      tDaTrieNode   val;

      status = datrieAllocate( 0, 0, 0, &hTrie );
      if ( eDaTrieStatus_Success == status )
      {
          // Ok-- trie allocated.  Charge the dictionary:
          datrieInsert( hTrie, "baby",     ( tDaTrieNode ) 1, false, 0 );
          datrieInsert( hTrie, "bachelor", ( tDaTrieNode ) 2, false, 0 );
          datrieInsert( hTrie, "back",     ( tDaTrieNode ) 3, false, 0 );
          datrieInsert( hTrie, "badge",    ( tDaTrieNode ) 4, false, 0 );
          datrieInsert( hTrie, "badger",   ( tDaTrieNode ) 5, false, 0 );
          datrieInsert( hTrie, "badness",  ( tDaTrieNode ) 6, false, 0 );
          datrieInsert( hTrie, "bcs",      ( tDaTrieNode ) 7, false, 0 );

          // These will all return eDaTrieStatus_Success.

          // Re-insertion behavior is governed by the fReplace flag:
          status = datrieInsert( hTrie, "baby", ( tDaTrieNode ) 8, false, 0 );
          assert( eDaTrieStatus_KeyAlreadyExists == status );

          status = datrieInsert( hTrie, "baby", ( tDaTrieNode ) 8, true, 0 );
          assert( eDaTrieStatus_Success == status );
          // "baby" now associated to 8

          // Match on "ba"-- fails because the key "ba" doesn't exist
          status = datrieMatch( hTrie, "ba", 0 );
          assert( eDaTrieStatus_Failure == status );

          // Search on "ba"-- succeeds because it does exist as a
          // prefix:
          status = datrieSearch( hTrie, "ba", 0, &n );
          assert( eDaTrieStatus_Success == status && 2 == n );

          // Match on "bachelorette-- fails:
          status = datrieMatch( hTrie, "bachelorette", 0 );
          assert( eDaTrieStatus_Failure == status );

          // Search on "bachelorette-- succeeds & returns the value
          // assoc'd with the longest matched prefix:
          status = datrieSearch( hTrie, "bachelorette", &val, &n );
          assert( eDaTrieStatus_Success == status &&
                  ( tDaTrieValue ) 2    == value  &&
                  8                     == n );

          datrieFree( hTrie );
      }

      return 0;
  }

 \endcode
 *
 *
 * \section datrie_ref References
 *
 * <ul>
 *
 * <li> \anchor datrie_ref_1 Aoe, Jun-Ichi "An Efficient Digital Search
 * Algorithm by Using a Double-Array Structure", IEEE Transactions On
 * Software Engineering. Vol. 15, No. 9. September 1989
 *
 * <li> \anchor datrie_ref_2 Knuth, Donald "The Art of Computer
 * Programming, Volume 3 Searching and Sorting", Second Edition, Chapter
 * 6.3 Digital Searching, pp492-496.
 *
 * </ul>
 *
 *
 */


////////////////////////////////////////////////////////////////////////
//                Types Exported by the datrie Module                 //
////////////////////////////////////////////////////////////////////////

// Forward declaration used below to define an ADT representing a trie
struct sDaTrie;

/// Abstract data type represeting a trie as exported by this module--
/// all APIs work in terms of this handle, rather than exposing the type
/// directly to callers.
typedef struct sDaTrie *hDaTrie;

/// Error codes returned by functions in this module
typedef enum eDaTrieStatus
{
    /// Successful return
    eDaTrieStatus_Success,
    /// The requested operation was unsuccessful; the exact semantics
    /// vary depending on the function
    eDaTrieStatus_Failure,
    /// The key supplied to an insert operation already existed in the
    /// trie, and replacement wasn't explicitly requested by the caller
    eDaTrieStatus_KeyAlreadyExists,
    /// Failed to allocate storage
    eDaTrieStatus_AllocationFailure,
    /// Trie needs re-allocation
    eDaTrieStatus_TrieNeedsGrow,
} tDaTrieStatus, *ptDaTrieStatus;

/// Typedef for the context passed into our allocation & de-allocation
/// functions
typedef void *tDaTrieContext;

#ifndef DATRIE_MALLOC
    /// Macro abstracting out memory allocation
#   define DATRIE_MALLOC( ctx, cb ) malloc( ( cb ) )
#   include <malloc.h>          // for malloc
#endif

#ifndef DATRIE_FREE
    /// Macro abstracting out memory de-allocation
#   define DATRIE_FREE( ctx, p ) free( ( p ) )
#   include <malloc.h>          // for malloc
#endif

#ifndef DATRIE_REALLOC
    /// Macro abstracting out memory re-allocation
#   define DATRIE_REALLOC( ctx, p, cb ) realloc( p, cb );
#   include <malloc.h>
#endif

#ifndef DATRIE_MEMCPY
    /// Macro abstracting out memory copying
#   define DATRIE_MEMCPY( ctx, dest, src, cb ) \
        memcpy( ( dest ), ( src ), ( cb ) )
#endif

#ifndef DATRIE_MEMSET
    /// Macro abstracting out memory initialization
#   define DATRIE_MEMSET( ctx, dest, val, cb ) \
        memset( ( dest ), ( val ), ( cb ) )
#endif

#ifndef DATRIE_STRLEN
    /// Macro abstracting out string length calculation
#   define DATRIE_STRLEN( ctx, s ) strlen( ( s ) )
#   include <string.h>          // For strlen
#endif

#ifndef DATRIE_STRCPY
    /// Macro abstracting out string copying-- strings may overlap, so
    /// strcpy & strncpy may not be used. memmove is guaranteed to work
    /// correctly even if the buffers overlap.
#   define DATRIE_STRCPY( ctx, dest, src ) \
        memmove( ( dest ), ( src ), DATRIE_STRLEN( ( ctx ), ( src ) ) + 1 )

#   include <string.h>          // For strlen
#endif

#ifndef DATRIE_ASSERT
    /// Assertion macro used internally by this module
#   define DATRIE_ASSERT( cond ) assert( ( cond ) )
#   include <assert.h>          // For assert
#endif

/// Tyepdef for the values associated with trie nodes
typedef void *tDaTrieNode;

/// Typedef for a visitor function-- use with datrieVisitNodes
typedef int ( *pfnDaTrieVisitor )( tDaTrieContext, tDaTrieNode );


////////////////////////////////////////////////////////////////////////
//              Functions Exported by the datrie Module               //
////////////////////////////////////////////////////////////////////////

/**
 * \brief Allocate storage for & initialize a new trie-- the caller is
 * responsible for freeing the memory by calling datrieFree
 *
 * \sa datrieFree
 *
 *
 * \param init Initial size of  the double-array, in terms of the number
 * of indicies to be allocated.  The  caller can pass zero to accept the
 * default setting (currently, this is  256 slots, but that's subject to
 * change).
 *
 * \param root Value to associate with the root node
 *
 * \param context Context value to be passed into this trie's memory
 * handling function pointers
 *
 * \param phTrie Pointer to an hDaTrie that, on successful return, will
 * be set to a handle that the caller can use when calling other datrie
 * functions
 *
 * \return eDaTrieStatus_Success on success, or
 * eDaTrieStatus_AllocationFailure on failure to allocate storage for
 * the trie
 *
 *
 * This will likely be the first method callers will invoke.  It creates
 * a new trie datastructure, initializes it & returns an opque handle to
 * that structure to the caller.   This handle may be used in subsequent
 * operations on the trie.
 *
 *
 */

tDaTrieStatus datrieAllocate( unsigned short       init,
                              tDaTrieNode          root,
                              tDaTrieContext       context,
                              hDaTrie             *phTrie );

/**
 * \brief Free the memory associated with a previously allocated trie
 *
 * \sa datrieAllocate
 *
 *
 * \param hTrie Handle to the trie to be deallocated
 *
 *
 * This method  will free the  trie datastructure itself; the  caller is
 * responsible  for managing  any  resources associated  with the  nodes
 * contained therein.
 *
 *
 */

void datrieFree( hDaTrie hTrie );

/**
 * \brief Insert a new key/value pair into the trie
 *
 *
 * \param hTrie Handle to the trie to be modified
 *
 * \param key A NULL-terminated string to be inserted into hTrie
 *
 * \param value Caller-supplied value to be associated with key in the
 * trie
 *
 * \param fReplace  Flag governing  this method's behavior  upon finding
 * that \a key already exists.  If \a key already exists, and the caller
 * sets this parameter to true, the new value will be associated with \a
 * key,   the   old   value   will   be  returned   in   *pExtant,   and
 * eDaTrieStatus_Success will be returned.  If the caller sets this flag
 * to false, the old value will  not be changed, *pExtant will be set to
 * nil, hTrie  won't be changed  and eDaTrieStatus_KeyAlreadyExists will
 * be returned.
 *
 * \param  pExtant Pointer  to a  hDaTrieNode which,  if \a  key already
 * exists in hTrie, and if the  caller has set fReplace to true, will be
 * set to point  to the old value associated with  key upon return.  The
 * caller may set this to zero if not interested.
 *
 * \return eDaTrieStatus_Success on success,
 * eDaTrieStatus_KeyAlreadyExists if lpszKey is already present in the
 * trie and fReplace is set to false.
 *
 *
 * Here's a quick chart describing the behavior of this method in
 * various circumstances:
 *
 \code
   Does 'key'
   already               pExtant upon  function's
   exist?      fReplace  return        exit status       Notes

   No          n/a       nil           Success
   Yes         false     nil          KeyAlreadyExists  No change to extant
                                                        value assoc'd with
                                                        "key"
   Yes         true      old value  Success             New value now
                         assoc'd w/                     assoc'd w/"key"
                         "key"

 \endcode
 *
 *
 */

tDaTrieStatus datrieInsert( hDaTrie      hTrie,
                            const char  *key,
                            tDaTrieNode  value,
                            int          fReplace,
                            tDaTrieNode *pExtant );

/**
 * \brief Lookup a key/value pair in the trie (exact match)
 *
 * \sa datrieSearch
 *
 *
 * \param hTrie Handle to the trie to be searched
 *
 * \param text NULL-terminated string on which to search
 *
 * \param  pValue Pointer  to a  hDaTrieNode  which, if  text exists  in
 * hTrie, will be  set to the value (if any)  associated with text.  The
 * caller can set this to 0, if not interested.
 *
 * \return eDaTrieStatus_Success on success, or eDaTrieStatus_Failure
 * else.
 *
 *
 * This method will  perform an exact text match  on text, returning the
 * value associated  with that node  if it's found.  For  longest prefix
 * matching, see datrieSearch.
 *
 *
 */

tDaTrieStatus datrieMatch( hDaTrie      hTrie,
                           const char  *text,
                           tDaTrieNode *pValue );

/**
 * \brief Search for a key/value pair in the trie (longest prefix)
 *
 * \sa datrieMatch
 *
 *
 * \param hTrie Handle to the trie to be searched
 *
 * \param text NULL-terminated string on which to search
 *
 * \param pValue  Pointer to  a hDaTrieNode which,  if a prefix  of text
 * exists in  hTrie, will be set  to the value (if  any) associated with
 * the  longest such  prefix.  The  caller may  set this  to nil  of not
 * interested.
 *
 * \param pnMatch Address  of a size_t to be set to  the # of characters
 * matched  in the  search.   The caller  may  set this  to  nil of  not
 * interested.
 *
 * \return eDaTrieStatus_Success on success, eDaTrieStatus_Failure if
 * \a text does note exist in the trie.
 *
 *
 * Use this method to do longest-prefix searching in hTrie.  To perform
 * exact matching, use datrieMatch.
 *
 *
 */

tDaTrieStatus datrieSearch( hDaTrie      hTrie,
                            const char  *text,
                            tDaTrieNode *pValue,
                            size_t      *pnMatch );

/**
 * \brief Visit all nodes in a trie, in no particular order
 *
 *
 * \param hTrie Handle to the trie to be visited
 *
 * \param pfnVisitor Pointer to the visiting function
 *
 * \returun eDaTrieStatus_Success
 *
 *
 * This function walks all nodes  in a trie, invoking pfnVisitor on each
 * (passing the  trie's context each  time), until either all  the nodes
 * have been visited, or the  visitor function returns false.  Note that
 * there is no guarantee on order!
 *
 *
 */

tDaTrieStatus datrieVisitNodes( hDaTrie hTrie, pfnDaTrieVisitor pfnVisitor );


#ifdef DATRIE_INCLUDE_DUMP_CODE

#   ifndef DATRIE_LOG0
#       include <stdio.h>
#       define DATRIE_LOG0( ctx, msg ) printf( ( msg ) )
#   endif

#   ifndef DATRIE_LOG1
#       include <stdio.h>
#       define DATRIE_LOG1( ctx, fmt, p0 ) printf( ( fmt ), ( p0 ) )
#   endif

#   ifndef DATRIE_LOG2
#       include <stdio.h>
#       define DATRIE_LOG2( ctx, fmt, p0, p1 ) \
            printf( ( fmt ), ( p0 ), ( p1 ) )
#   endif

#   ifndef DATRIE_LOG3
#       include <stdio.h>
#       define DATRIE_LOG3( ctx, fmt, p0, p1, p2 ) \
            printf( ( fmt ), ( p0 ), ( p1 ), ( p2 ) )
#   endif

#   ifndef DATRIE_LOG4
#       include <stdio.h>
#       define DATRIE_LOG4( ctx, fmt, p0, p1, p2, p3 )     \
            printf( ( fmt ), ( p0 ), ( p1 ), ( p2 ), ( p3 ) )
#   endif

/**
 * \brief Dump a trie
 *
 *
 * \param hTrie Handle to a trie to be dumpted
 *
 *
 * This method  will dump  the underlying trie  datastructure (generally
 * for  debugging  purposes).   The  destination  for the  log  text  is
 * determined by the DATRIE_LOG* macros.
 *
 *
 */

void datrieDump( hDaTrie hTrie );

#endif // DATRIE_INCLUDE_DUMP_CODE


// Local Variables:
// fill-column: 72
// indent-tabs-mode: nil
// show-trailing-whitespace: t
// End:

#endif // DATRIE_H_DFD2ADCA_97C3_4373_8007_DB6687FECA95
