/**
 * \file datrie.c
 *
 * \brief Implementation file for the 'datrie' module
 *
 *
 * This is  the implementation file for  datrie, a C  module providing a
 * generic  implementation of  a double-array  trie  implementation (see
 * \ref datrie "here").
 *
 *
 */

#include "datrie.h"             // Module header file


////////////////////////////////////////////////////////////////////////
//         Private Constants & Types Internal to this Module          //
////////////////////////////////////////////////////////////////////////

#define DEFAULT_INITIAL_SIZE ( 256 )

/// Double-array trie
typedef struct sDaTrie
{
    /// Array BASE
    int *base;
    /// Array CHECK; note that the current size of the array is stored in
    /// check[ 0 ]
    int *check;
    /// Application-specific value associated with each node
    tDaTrieNode *node;
    /// String memory
    char *tail;
    /// String memory size
    size_t ntail;
    /// Minimum index of available entries of TAIL
    int pos;
    /// Context (if any)
    tDaTrieContext context;
} tDaTrie, *ptDaTrie;

/// Sentiel value in 'tail' indicating an unusd position
#define EMPTY ( 1 )

/// Helper macro for converting an ASCII character to an input value
#define I4A( p, i ) ( *( p + i ) == 0 ? 0 : *( p + i ) - ' ' + 1 )


////////////////////////////////////////////////////////////////////////
//               Functions Private to the datrie Module               //
////////////////////////////////////////////////////////////////////////

/// Insert a new arc into a trie
static tDaTrieStatus a_insert( ptDaTrie       pTrie,
                               unsigned short r,
                               const char    *lpszRemainder,
                               tDaTrieNode    value );

/// Break a single-node into two
static tDaTrieStatus b_insert( ptDaTrie       pTrie,
                               unsigned short r,
                               const char    *lpszSingle,
                               const char    *lpszRemainder,
                               size_t         k,
                               tDaTrieNode    value );

/// Core trie traversal routine
static tDaTrieStatus walk_trie( ptDaTrie     pTrie,
                                const char  *text,
                                tDaTrieNode  value,
                                int          fInsert,
                                int          fReplace,
                                tDaTrieNode *pValue,
                                size_t      *pnMatch,
                                tDaTrieNode *pLast,
                                size_t      *pnLast );

/// Retrieve a single-string from storage
static const char * fetchstr( ptDaTrie pTrie, int i );

/// Grow the trie, possibly to a minimum new size
static tDaTrieStatus grow( ptDaTrie       pTrie,
                           unsigned short nNew );

/// Define an arc to a new single-node
static tDaTrieStatus insstr( ptDaTrie       pTrie,
                             unsigned short r,
                             const char    *lpszRemainder,
                             tDaTrieNode    value,
                             int            pos );

/// Modify a trie so as to accomodate a new transition
static tDaTrieStatus modify( ptDaTrie        pTrie,
                             unsigned short  nCurrent,
                             unsigned short  h,
                             char            add,
                             char           *porg,
                             size_t          norg,
                             unsigned short *ph );

/// Return the set of symbols a such that CHECK[ BASE[ r ] + a ] = r
static void set_list( ptDaTrie       pTrie,
                      unsigned short r,
                      char         **pprlist,
                      size_t        *pnrlist );

/// Return - 1 if a string x is equal to string y, otherwise return the
/// length of the longest prefix of x and y.
static int str_cmp( const char *lhs, const char *rhs );

/// Store a string in location pos of TAIL
static tDaTrieStatus strtail( ptDaTrie     pTrie,
                              int          pos,
                              const char *lpszText );

/// Return the minimum q such that q > 0 and CHECK[ q + c ] = 0 for all
/// c in { add } U porg
static tDaTrieStatus x_check( ptDaTrie        pTrie,
                              char            add,
                              char           *porg,
                              size_t          norg,
                              unsigned short *pnew_base );

/**
 * \brief Core trie traversal routine
 *
 *
 * \param pTrie Address of the trie to be traversed
 *
 * \param text Text string driving the traversal
 *
 * \param pValue Value to be associated  with \a text, \em if the caller
 * has  requested  insertion as  opposed  to  searching  (on which  more
 * below).
 *
 * \param  fInsert  This  flag  controls whether  this  subroutine  will
 * mereley  search or search  and insert.   If the  caller sets  this to
 * zero, then this  method will act solely as  a search algorithm.  That
 * is,  it will  stop as  soon as  search failure  is detected  & return
 * eDaTrieStatus_Failure (if \a text exists  in the trie, it will return
 * eDaTrieStatus_Success).  If, however,  the caller supplies a non-zero
 * value, this method will attempt to  insert \a text into the trie, and
 * associate  pValue with  that string.   If the  search  string already
 * exists, the behavior is governed by the fReplace parameter.
 *
 * \param  fReplace  This  flag  comes  into play  when  the  caller  as
 * requested insertion, \em and when \a text already exists in the trie.
 * If this  parameter is  zero, this function  will do nothing  & return
 * eDaTrieStatus_KeyAlreadyExists.   If this  paramter is  non-zero, the
 * old value  associated with  \a text will  be returned in  pExtant and
 * pValue will now be associated with \a text.
 *
 * \param pExtant  Address of  an tDaTrieNode that,  on return,  will be
 * filled in with the node  associated with the longest match found, or,
 * if replacing, the node previously associated with the text string, if
 * any.
 *
 * \param pnMatch  The number of  characters in text matched  during the
 * search
 *
 * \param pLast If  fInsert is zero (i.e. this  function is being called
 * to search, not to insert)  this routine will match the longest prefix
 * of \a text possible.  If that prefix has no value associated with it,
 * this routine will then backtrack & find the longest prefix of \a text
 * that \em  is associated with a  value, and return that  value in this
 * variable
 *
 * \param pnLast The number of characters matched for the value in pLast
 *
 * \return See the table, below
 *
 *
 * This  subroutine is  the core  traversal algorithm,  essentially what
 * \ref datrie_ref_1 "Aoe" calls Algorithm 2.  Given a search string, it
 * will traverse the  trie interpreting each character as  an input to a
 * DFA.   Upon walking  the datastructure  as far  as possible,  it will
 * return or insert a new value to be associated with the search string,
 * depending on the caller's preferences.
 *
 * Here's a table summarizing the possibilities:
 *
 \code
                    Search
 fInsert  fReplace  Result  Return          pExtant

 false    false     partial _Failure        Node assoc'd w/longest prefix
 false    false     exact   _Success        Node assoc'd w/match
 false    true      partial _Failure        Node assoc'd w/longest prefix
 false    true      exact   _Success        Node assoc'd w/match
 true     false     partial _Success        Node assoc'd w/longest prefix [1]
 true     false     exact   _KeyAlready...  Node assoc'd w/match
 true     true      partial _Success        Node assoc'd w/longest prefix [1]
 true     true      exact   _Success        Node prev'ly assoc'd w/match  [2]

 \endcode
 *
 * Notes:
 *
 * - The search string will be inserted into the trie, and associated
 *   with pValue
 *
 * - The search string is \em now associated with pValue
 *
 *
 * This subroutine  does not  exactly conform to  that laid out  in \ref
 * datrie_ref_1 "Aoe".  He terminates the  iteration on a bad value of t
 * solely.  However, there are two cases (at least) in which this is not
 * sufficient:
 *
 * - The string is empty, which we would like to correspond to the root
 *   node
 *
 * - The string is  a prefix of another key in  the trie.  For instance,
 *   in  the \ref  datrie_naive "example",  take the  string  "badge" (a
 *   prefix  of  another  key   in  the  trie,  "badger").   After  five
 *   iterations, t will  be 11, and BASE[ t ] is  1-- however, we're out
 *   of characters in the search string!
 *
 * Consequently, I've modified the loop to terminate on string
 * exhaustion, too.
 *
 *
 */

static tDaTrieStatus walk_trie( ptDaTrie     pTrie,
                                const char  *text,
                                tDaTrieNode  pValue,
                                int          fInsert,
                                int          fReplace,
                                tDaTrieNode *pExtant,
                                size_t      *pnMatch,
                                tDaTrieNode *pLast,
                                size_t      *pnLast )
{
    unsigned short r;           // Current node number
    unsigned short t;           // Provisional next node number
    unsigned short k;
    size_t         i, h;        // Current index into 'text'
    size_t         ntext;       // Length of "text"
    const char    *tmp;

    ntext = DATRIE_STRLEN( pTrie->context, text );

    r = 1;                      // Initialize the current node number to
                                // the initial state
    h = 0;                      // Initialize the current input
                                // character to the beginning of the
                                // string
    // Ok-- walk the trie's multi-nodes for as long as we can:
    for ( i = 0; i < ntext; ++i )
    {
        // Set the (provisional) next node to BASE of this node plus the
        // current input:
        t = pTrie->base[ r ] + I4A( text, h );
        // Check: is this a valid transition?
        if ( t >= pTrie->check[ 0 ] || pTrie->check[ t ] != r )
        {
            // It is not.  The search has failed.  In any event, h is
            // equal to the number of characters we were able to match,
            // so we can fill out *that* output:
            *pnMatch = h;
            // And what the heck-- we lookup the value associated with
            // the prefix we *did* match, too.
            *pExtant = pTrie->node[ r ];

            // What happens next depends on the caller's settings.  If
            // they specified "insert on search failure"...
            if ( fInsert )
            {
                // then we're going to insert a new arc:

                //   g( s(r), a(h) ) = s(t)

                // for some t, and a new single-string:

                //   STR[ s(t) ] = a(h+1)...a(n)

                return a_insert( pTrie, r, text + h, pValue );
            }
            else
            {
                // The caller did *not* request insertion.  That's
                // fine-- we're almost done.  We've figured out how many
                // characters we *have* matched, and we've recorded the
                // value associated with this prefix.  Now, we just need
                // to figure out the last prefix that actually had a
                // value associated with it:
                if ( *pExtant )
                {
                    // Well, that's easy, it's our current node!
                    *pLast  = *pExtant;
                    *pnLast = *pnMatch;
                }
                else
                {
                    // We have to walk the trie *back* up to the most
                    // recent node having a value:
                    *pLast  = 0;
                    *pnLast = 0;
                    while ( h )
                    {
                        r = pTrie->check[ r ];
                        if ( pTrie->node[ r ] )
                        {
                            *pLast  = pTrie->node[ r ];
                            *pnLast = h;
                            break;
                        }
                        h--;
                    }

                }

                // Finally, we just return the appropriate status code:
                return eDaTrieStatus_Failure;
            }
        }

        // Yes-- this is a valid transition.
        r  = t;                 // Move to state t,
        h += 1;                 // and advance to the next character.

        if ( pTrie->base[ r ] < 0 ) break;

    } // End iteration over 'text'.

    // Ok-- we could be here for one of two reasons:

    // 1. We've run out of search text.  That means "text" is a prefix
    // to some other key in this trie

    // 2. BASE[ r ] < 0.  That means we've walked the trie down to a
    // single-node.

    if ( pTrie->base[ r ] >= 0 )
    {
        // Case 1: The text string happens to be a prefix of another key
        // in this trie.

        // Lookup the value associated with this node,
        *pExtant = pTrie->node[ r ];
        // note the number of characters we matched (all of 'em),
        *pnMatch = ntext;
        // & return the appropriate error code.
        if ( fInsert )
        {
            if ( fReplace )
            {
                pTrie->node[ r ] = pValue;
                return eDaTrieStatus_Success;
            }
            else
            {
                return eDaTrieStatus_KeyAlreadyExists;
            }

        }
        else
        {
            if ( *pExtant )
            {
                // Well, that's easy, it's our current node!
                *pLast  = *pExtant;
                *pnLast = *pnMatch;
            }
            else
            {
                // We have to walk the trie *back* up to the most
                // recent node having a value:
                *pLast  = 0;
                *pnLast = 0;
                while ( h )
                {
                    r = pTrie->check[ r ];
                    if ( pTrie->node[ r ] )
                    {
                        *pLast  = pTrie->node[ r ];
                        *pnLast = h;
                        break;
                    }
                    h--;
                }

            }

            return *pExtant ? eDaTrieStatus_Success : eDaTrieStatus_Failure;
        }
    }

    // Case 2: BASE[ r ] < 0.  Fetch STR[ s(r) ],
    tmp = fetchstr( pTrie, pTrie->base[ r ] );

    // & compare it to the remaining input:
    k = str_cmp( text + h, tmp );
    // Again, check the simple case: do they match?
    if ( 0xffff == k )
    {
        // They do.  Lookup the value associated with this node,
        *pExtant = pTrie->node[ r ];
        // note the number of characters we matched (all of 'em),
        *pnMatch = h + DATRIE_STRLEN( pTrie->context, tmp );
        // & return the appropriate error code.
        if ( fInsert )
        {
            if ( fReplace )
            {
                pTrie->node[ r ] = pValue;
                return eDaTrieStatus_Success;
            }
            else
            {
                return eDaTrieStatus_KeyAlreadyExists;
            }

        }
        else
        {
            return eDaTrieStatus_Success;
        }
    }

    // Finally, if we get to this point, the search has failed.  Did the
    // caller specify "insert-on-search-failure"?
    if ( fInsert )
    {
        // They did.  Make s(r) a multi-node by inserting two new
        // single-nodes:
        return b_insert( pTrie, r, tmp, text + h, k, pValue );
    }
    else
    {
        // The did not-- note the number of characters we *were* able to
        // match:
        *pnMatch = h + k;
        // lookup the value associated with the prefix we matched,
        *pExtant = pTrie->node[ r ];

        if ( *pExtant )
        {
            // Well, that's easy, it's our current node!
            *pLast  = *pExtant;
            *pnLast = *pnMatch;
        }
        else
        {
            // We have to walk the trie *back* up to the most
            // recent node having a value:
            *pLast  = 0;
            *pnLast = 0;
            while ( h )
            {
                r = pTrie->check[ r ];
                if ( pTrie->node[ r ] )
                {
                    *pLast  = pTrie->node[ r ];
                    *pnLast = h;
                    break;
                }
                h--;
            }

        }

        // & return the appropriate error code.
        return eDaTrieStatus_Failure;
    }

} // End walk_trie.

/**
 * \brief Insert a new arc into a trie
 *
 *
 * \param pTrie trie on which we're to operate
 *
 * \param r Node number, or state, to which the arc is to be added
 *
 * \param lpszRemainder The  text to be appended to  the prefix to which
 * node r corresponds (this will be the search text less the prefix that
 * \em was matched)
 *
 * \param  pValue Caller-supplied value  to be  associated with  the new
 * string (i.e. it will be stuffed into NODE[ t ]).
 *
 * \return eDaTrieStatus_Success on success, or
 * eDaTrieStatus_AllocationFailure on failure to allocate sufficient
 * storage for the new string
 *
 *
 * This is procedure A-INSERT in \ref datrie_ref1 "Aoe".  It will add an
 * arc from s(r) to a new single-node in the trie on the first character
 * of lpszRemainder, and  then set the STR for that node  to the rest of
 * lpszRemainder.
 *
 * Pictorially:
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
 *
 */

static tDaTrieStatus a_insert( ptDaTrie       pTrie,
                               unsigned short r,
                               const char    *lpszRemainder,
                               tDaTrieNode    pValue )
{
    unsigned short t;           // Provisional new node
    unsigned short k;           // Node number conflicting with r
    char          *rlist, *klist;
    size_t         nrlist, nklist;
    tDaTrieStatus  status;

    // It would be *really* nice if we could just slot the new input
    // character into the double-array as it stands; here's what we'd
    // *like* the new node number to be:
    t = pTrie->base[ r ] + I4A( lpszRemainder, 0 );
    // but is it too big?
    if ( t >= pTrie->check[ 0 ] )
    {
        status = grow( pTrie, t );
        if ( eDaTrieStatus_Success != status ) return status;
    }
    // and is that node number empty?
    else if ( pTrie->check[ t ] )
    {
        // *sigh* It is not.  We're going to have to re-jigger things.
        // We're going to need to change either BASE[ r ] or BASE[ k ]
        // where k is the node number using slot t in our double-array
        // (i.e. CHECK[ t ]).
        k = pTrie->check[ t ];
        // The choice of which node to re-base depends on how many
        // transitions each has coming out of it; call SET-LIST to
        // figure that out:
        set_list( pTrie, r, &rlist, &nrlist );
        set_list( pTrie, k, &klist, &nklist );

        if ( nrlist + 1 < nklist )
        {
            // It will be less work to re-base r:
            status = modify( pTrie, r, r, *lpszRemainder,
                             rlist, nrlist, &t );
        }
        else
        {
            // It will be less work to re-base k:
            status = modify( pTrie, r, k, 0, klist, nklist, &r );
        }

        DATRIE_FREE( pTrie->context, rlist );
        DATRIE_FREE( pTrie->context, klist );

        // In Progress: Document modify's possible return values.
        if ( eDaTrieStatus_Success != status ) return status;
    }

    return insstr( pTrie, r, lpszRemainder, pValue, pTrie->pos );

} // End a_insert.

/**
 * \brief Break a single-node into two
 *
 *
 * \param pTrie Address of the trie on which this function shall operate
 *
 * \param r Node number of the single-node to be sub-divided
 *
 * \param single Single-string for node r (i.e. STR[ s(r) ])
 *
 * \param remainder Text being inserted into the trie, without the
 * prefix to which node r corresponds
 *
 * \param k Length of the longest shared prefix between remainder
 * and single
 *
 * \param pValue Application-specific value to be associated with the
 * new single-node (s(t'), below)
 *
 * \return eDaTrieStatus_Success on success
 *
 *
 * This  function  is  called  during  sting  insertion  when  the  trie
 * traversal has reached a  single-node whose single-string does \em not
 * match remainder.  Pictorially, this is the trie as it stands now:
 *
 \code
        a(1)...a(h)
  s(1) ------------> s(r)

        STR[ s(r) ] = a(h+1)...a(h+k)b(1)...b(m)

 \endcode
 *
 * and here is the text:
 *
 \code

  Input:    a(1)a(2)...a(h)a(h+1)...a(h+k)a(h+k+1)...a(n)
  STR[s(r)]:               a(h+1)...a(h+k)b(1) ...   b(m)
                           <--  same  -->  ^
                                           |
               first different character---
 \endcode
 *
 * So, to relate this schematic to our parameters, single is
 * a(h+1)...a(h+k)b(1)...b(m).  remainder is a(h+1)...a(n).  This
 * method will insert two new single-nodes, like so:
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
 */

static tDaTrieStatus b_insert( ptDaTrie       pTrie,
                               unsigned short r,
                               const char    *single,
                               const char    *remainder,
                               size_t         k,
                               tDaTrieNode    pValue )
{
    size_t         i;
    char           c, b;
    int            old_pos;   // Old index into TAIL for STR[ s(r) ]
    void          *pOldValue; // Old value associated with s(r)
    unsigned short new_base;
    tDaTrieStatus  status;

    old_pos          = - pTrie->base[ r ];
    pOldValue        = pTrie->node[ r ];
    pTrie->node[ r ] = 0;

    // We define a sequence of arcs for each a(h+i) for 0 <= i < k on
    // the double-array:
    for ( i = 0; i < k; ++i )
    {
        c = I4A( remainder, i );
        status = x_check( pTrie, 0, &c, 1, &new_base );
        if ( eDaTrieStatus_Success != status ) return status;

        pTrie->base[ r ] = new_base;
        pTrie->check[ pTrie->base[ r ] + c ] = r;
        r = pTrie->base[ r ] + c;
    }

    c = I4A( remainder, k );
    b = I4A( single, k );
    status = x_check( pTrie, b, &c, c ? 1 : 0, &new_base );
    if ( eDaTrieStatus_Success != status ) return status;

    pTrie->base[ r ] = new_base;
    status = insstr( pTrie, r, single + k, pOldValue, old_pos );
    if ( eDaTrieStatus_Success != status ) return status;

    status = insstr( pTrie, r, remainder + k, pValue, pTrie->pos );
    if ( eDaTrieStatus_Success != status ) return status;

    return eDaTrieStatus_Success;

} // End b_insert.

/**
 * \brief Grow the trie, possibly to a minimum new size
 *
 *
 * \param pTrie Address of the trie which this function shall grow
 *
 * \param nNew Minium new size; the caller may set this to zero to
 * accept a default
 *
 * \return eDaTrieStatus_Success on success,
 * eDaTrieStatus_AllocationFailure on memory allocation failure
 *
 *
 */

static tDaTrieStatus grow( ptDaTrie       pTrie,
                           unsigned short nNew )
{
    size_t          cb;
    unsigned short  slots, oldslots;
    void           *pb, *pc, *pn;

    // The default is to just double the size
    slots = pTrie->check[ 0 ] * 2;
    slots = slots < nNew ? nNew : slots;

    cb = slots * sizeof( int );

    pb = DATRIE_REALLOC( pTrie->context, pTrie->base, cb );
    if ( ! pb ) return eDaTrieStatus_AllocationFailure;

    pc = DATRIE_REALLOC( pTrie->context, pTrie->check, cb );
    if ( ! pc )
    {
        DATRIE_FREE( pTrie->context, pb );
        return eDaTrieStatus_AllocationFailure;
    }

    cb = slots * sizeof( tDaTrieNode );

    pn = DATRIE_REALLOC( pTrie->context, pTrie->node, cb );
    if ( ! pc )
    {
        DATRIE_FREE( pTrie->context, pb );
        DATRIE_FREE( pTrie->context, pc );
        return eDaTrieStatus_AllocationFailure;
    }

    pTrie->base  = pb;
    pTrie->check = pc;
    pTrie->node  = pn;

    oldslots = pTrie->check[ 0 ];

    pTrie->check[ 0 ] = slots;

    cb = ( slots - oldslots ) * sizeof( int );
    DATRIE_MEMSET( pTrie->context, pTrie->base + oldslots, 0, cb );
    DATRIE_MEMSET( pTrie->context, pTrie->check + oldslots, 0, cb );

    cb = ( slots - oldslots ) * sizeof( tDaTrieNode );
    DATRIE_MEMSET( pTrie->context, pTrie->node + oldslots, 0, cb );

    return eDaTrieStatus_Success;

} // End grow.

/**
 * \brief Retrieve a single-string from storage
 *
 *
 * \param pTrie Address of the trie on which we're to operate
 *
 * \param i Index into TAIL at which the string begins
 *
 * \return The NULL-terminated string stored at index -i
 *
 * \pre i < 0
 *
 *
 */

static const char * fetchstr( ptDaTrie pTrie,
                              int      i )
{
    DATRIE_ASSERT( i < 0 );
    return &( pTrie->tail[ -i ] );
} // End fetchstr.

/**
 * \brief Define an arc to a new single-node
 *
 *
 * \param pTrie Address of the trie on which we're to operate
 *
 * \param r Node number (or state) from which the new arc shall
 * originate
 *
 * \param lpszRemainder Remainder of the current insertion string,
 * without the prefix to which s(r) corresponds
 *
 * \param pValue Application-defined value to associate with the new node
 *
 * \param pos Index into TAIL at which the single-string shall be stored
 *
 * \return eDaTrieStatus_Success on success,
 * eDaTrieStatus_AllocationFailure on failure to allocate sufficient
 * storage for the new string
 *
 * \pre BASE[ BASE[ r ] ] + I4A( lpszRemainder[ 0 ] ) == 0
 *
 *
 */

static tDaTrieStatus insstr( ptDaTrie       pTrie,
                             unsigned short r,
                             const char    *lpszRemainder,
                             void          *pValue,
                             int            pos )
{
    unsigned short t;           // Next node number

    if ( ! DATRIE_STRLEN( pTrie->context, lpszRemainder ) )
    {
        pTrie->node[ r ] = pValue;
        return eDaTrieStatus_Success;
    }

    // t will be the next state when shifting out of state r on input
    // lpszRemainder[ 0 ]:
    t = pTrie->base[ r ] + I4A( lpszRemainder, 0 );
    // In Progress: Define this macro!
    DATRIE_ASSERT( ! pTrie->base [ t ] );

    // Setup state t: BASE[ t ] will be set to point to it's singlestring,
    pTrie->base [ t ] = - pos;
    // CHECK[ t ], of course, has to name our source state,
    pTrie->check[ t ] = r;
    // and NODE[ t ] will be associated with the caller-supplied value.
    pTrie->node [ t ] = pValue;

    // Finally, we delegate to 'strtail' to actually insert the
    // singlestring into our text storage:
    return strtail( pTrie, pos, lpszRemainder + 1 );

} // End datrieInsStr.

/**
 * \brief Modify a trie so as to accomodate a new transition
 *
 *
 * \param pTrie Tree on which this function shall operate
 *
 * \param current Node number denoting the state from which we'd like to
 * add a new transition
 *
 * \param h Node number denoting the state which the caller wants to
 * "re-base" (may be the same as current!)
 *
 * \param add New character on which to transition; if the caller is
 * re-basing the current node.  If the caller has chosen to re-base node
 * k (see below), they shall set this to zero.  Note that this is
 * expected to be the raw character!
 *
 * \param porg The set of extant transitions out of the state being rebased
 *
 * \param norg The size of porg
 *
 * \param ph The new value of our current node (on which more, below).
 *
 * \return eDaTrieStatus_Success on success, eDaTrieStatus_TrieNeedsGrow if the
 * trie needs to be "grown"
 *
 *
 * Insertion routines  will need to call  this method when  they need to
 * insert a  transition a out of  state r, but BASE[  r ] + a  != 0.  In
 * that case,  they'll need  to "re-base" either  state r, or  the state
 * using  BASE[ r  ] +  a.  The  caller is  responsible for  making that
 * decision  (presumably  on the  basis  of  which  would require  fewer
 * changes to the trie).  Pictorially:
 *
 \code

    BASE       CHECK

  k      ---
            |
  r      -  |
          | b
          a |
          | |
  t        - ->  k

 \endcode
 *
 * Where g( s(k), b ) = s(t) is already defined, and we'd \em like to
 * define g( s(r), a ).
 *
 * Note that when  re-basing state k, we must  consider the special case
 * where BASE[ k ] + d = r for d in K-LIST(i.e the set of symbols a such
 * that CHECK[ BASE[ k ] + a ] = k, or the set of valid inputs for state
 * k), because the current node number r is changed along with BASE[ k ]
 * as shown here:
 *
 \code

    BASE       CHECK

  k      --- -
            | |
            | d
            | |
  r      -  |  ->k
          | b
          a |
          | |
  t        - ->  k

 \endcode
 *
 *
 */

static tDaTrieStatus modify( ptDaTrie        pTrie,
                             unsigned short  current,
                             unsigned short  h,
                             char            add,
                             char           *porg,
                             size_t          norg,
                             unsigned short *ph )
{
    size_t         i;
    int            j;
    unsigned short old_base, new_base, t, tprime;
    tDaTrieStatus  status;

    if ( add ) add = I4A( &add, 0 );

    // Ok-- let's get to work.  We're to re-base node h.  Save off it's
    // old base (i.e. BASE[ ]),
    old_base = pTrie->base[ h ];
    // and search for a new base (returned in the variable 'new_base'):
    status = x_check( pTrie, add, porg, norg, &new_base );
    if ( eDaTrieStatus_Success != status ) return status;

    // As far as node h goes-- that's it:
    pTrie->base[ h ] = new_base;

    // However, we now need to update all the nodes reachable *from*
    // node h:
    for ( i = 0; i < norg; ++i )
    {
        t      = old_base + porg[ i ]; // The old destination on porg(i)
        tprime = new_base + porg[ i ]; // the *new* destination.

        pTrie->base [ tprime ] = pTrie->base[ t ]; // Copy BASE,
        pTrie->check[ tprime ] = h;                // CHECK,
        pTrie->node [ tprime ] = pTrie->node[ t ]; // & NODE down to the
                                                   // new destination.
        // Next, if *this* state in turn has transitions out of it...
        if ( pTrie->base[ t ] > 0 )
        {
            // re-set *their* CHECK values:
            for ( j = 1; j < 128; ++j )
            {
                if ( pTrie->check[ pTrie->base[ t ] + j ] == t )
                {
                    pTrie->check[ pTrie->base[ t ] + j ] = tprime;
                }
            }

        }

        // Finally, our special case (see this function's documentation):
        if ( t == current ) current = tprime;

        pTrie->base [ t ] = 0;
        pTrie->check[ t ] = 0;
        pTrie->node [ t ] = 0;
    }

    *ph = current;

    return eDaTrieStatus_Success;

} // End datrieModify.

/**
 * \brief Return the set of symbols a such that CHECK[ BASE[ r ] + a ] = r
 *
 *
 * \param pTrie Trie on which this method shall operate
 *
 * \param r Node number of interest
 *
 * \param pprlist On success, this parameter  will be set to an array of
 * char containing the  inputs for which transitions exist  out of state
 * r.  The caller is responsible for freeing this memory.  If the set of
 * such characters is empty, this parameter will be set to zero.
 *
 * \param pnrlist On success, the  size of the returned set.  The caller
 * should note that this may be zero.
 *
 *
 */

static void set_list( ptDaTrie       pTrie,
                      unsigned short r,
                      char         **pprlist,
                      size_t        *pnrlist )
{
    int    i;
    size_t n;

    // Walk the list once to size the required array:
    for ( i = 0, n = 0; i < pTrie->check[ 0 ]; ++i )
    {
        if ( pTrie->check[ i ] == r ) ++n;
    }

    if ( ! n )
    {
        *pprlist = 0;
        *pnrlist = 0;
        return;
    }

    *pnrlist = n;
    *pprlist = ( char* ) malloc( n );
    for ( i = 0, n = 0; i < pTrie->check[ 0 ]; ++i )
    {
        if ( pTrie->check[ i ] == r )
        {
            ( *pprlist )[ n++ ] = i - pTrie->base[ r ];
        }
    }

} // End set_list.

/**
 * \brief Return - 1 if a string x is equal to string y, otherwise
 * return the length of the longest prefix of x and y.
 *
 *
 * \param x Left-hand side of the comparison
 *
 * \param y Right-hand side of the comparison
 *
 * \return -1 if the two strings are identical, the length of the the
 * longest shared prefix else
 *
 *
 */

static int str_cmp( const char *x,
                    const char *y )
{
    int n = 0;

    while ( *x && *y )
    {
        if ( *x != *y ) break;
        ++n;
        ++x;
        ++y;
    }

    if ( ! *x && ! *y ) return -1;

    return n;

} // End datrieStrCmp.

/**
 * \brief Store a string in location pos of TAIL
 *
 *
 * \param pTrie Trie on which we're to operate
 *
 * \param  pos The position  at which  text shall  be inserted.   If the
 * caller sets this to POS (i.e. pTrie->pos), this routine will look for
 * the next slot  in the trie's text storage  large enough to accomodate
 * the text  to be inserted.   Else, the string will  be unconditionally
 * copied at position pos.
 *
 * \param text Text to be inserted
 *
 * \return eDaTrieStatus_Success on success,
 * eDaTrieStatus_AllocationFailure on failure to grow the trie's text
 * storage (should it be necessary).
 *
 *
 * If the caller passes pTrie->pos, this routine will update it
 * accordingly.
 *
 *
 */

static tDaTrieStatus strtail( ptDaTrie    pTrie,
                              int         pos,
                              const char *text )
{
    char  *p;
    size_t n;

    if ( pTrie->pos == pos )
    {
        n = DATRIE_STRLEN( pTrie->context, text );
        if ( pos + n > pTrie->ntail )
        {
            p = DATRIE_REALLOC( pTrie->context, pTrie->tail,
                                pTrie->ntail * 2 );
            if ( ! p ) return eDaTrieStatus_AllocationFailure;

            pTrie->tail   = p;
            pTrie->ntail *= 2;
        }

        DATRIE_STRCPY( pTrie->context, pTrie->tail + pos, text );
        pTrie->pos += ( int ) n + 1;
    }
    else
    {
        DATRIE_STRCPY( pTrie->context, pTrie->tail + pos, text );
    }

    return eDaTrieStatus_Success;

} // End strtail.

/**
 * \brief Return the minimum q such that q > 0 and CHECK[ q + c ] = 0
 * for all c in add U porg
 *
 *
 * \param pTrie Address of the trie on which this function shall operate
 *
 * \param add Optional additional transition to check (the caller shall
 * set this to zero to indicate "none")
 *
 * \param porg Pointer to a buffer of char defining extant transitions
 *
 * \param org Size of the buffer to which porg points
 *
 * \param pnew_base Address of a variable which, on success, will be set
 * to the new node number
 *
 * \return eDaTrieStatus_Success, eDaTrieStatus_AllocationFailure on
 * failure to grow the trie sufficiently
 *
 *
 * This method is  \ref datrie_ref1 "Aoe"'s method X-CHECK.   It is used
 * to "re-base"  a node;  that is, given  a state  h, we want  to change
 * BASE[ h ].  In order to do so,  we need to find a new base value such
 * that CHECK[ BASE[  h ] + c  ] is zero for all  c defining transitions
 * out of state h.
 *
 *
 * \note If this method cannot find a new base, it will grow the trie.
 *
 *
 */

static tDaTrieStatus x_check( ptDaTrie        pTrie,
                              char            add,
                              char           *porg,
                              size_t          norg,
                              unsigned short *pnew_base )
{
    unsigned short i;
    size_t         j;
    tDaTrieStatus  status;

    for ( ; ; )
    {
        // Ok, my approach is very simple.  I'm just going to walk each
        // index in the double-array (skipping the first, which is always
        // allocated to the root node)...
        for ( i = 1; i < pTrie->check[ 0 ]; ++i )
        {
            // and for each index in the double-array, I'm going to walk the
            // set of input events porg...
            for ( j = 0; j < norg; ++ j )
            {
                // We want to examine CHECK[ i + porg[ j ] ]:
                if ( i + porg[ j ] >= pTrie->check[ 0 ] ) break; // overflow
                if ( pTrie->check[ i + porg[ j ] ] )      break; // in use
            }

            // If we're here, either we've broken out of the loop above, or
            // we're done.  If we completed the loop...
            if ( j == norg )
            {
                // Check the last character 'add', if it's there...
                if ( ! add || 0 == pTrie->check[ i + add ] )
                {
                    *pnew_base = i;
                    return eDaTrieStatus_Success;
                }
            } // End if on completing the loop, above.
        }

        // If we're here, we've failed-- grow the trie & try again:
        status = grow( pTrie, 0 );
        if ( eDaTrieStatus_Success != status ) return status;
    }

} // End x_check.


////////////////////////////////////////////////////////////////////////
//              Functions Exported by the datrie Module               //
////////////////////////////////////////////////////////////////////////

/// Allocate storage for & initialize a new trie-- the caller is
/// responsible for freeing the memory by calling datrieFree
tDaTrieStatus datrieAllocate( unsigned short  init,
                              tDaTrieNode     root,
                              tDaTrieContext  context,
                              hDaTrie        *phTrie )
{
    size_t        n;
    ptDaTrie      pTrie  = 0;
    tDaTrieStatus status = eDaTrieStatus_AllocationFailure;

    *phTrie = 0;                // Zero-initialize our [out] parameter

    do
    {
        if ( ! init ) init = DEFAULT_INITIAL_SIZE;

        // Allocate the storage for the datastructure
        pTrie = ( ptDaTrie ) DATRIE_MALLOC( context, sizeof( tDaTrie ) );
        if ( ! pTrie ) break;

        // Take care to NULL out all members, as we'll use zero as a
        // sentinel value if we need to cleanup, below.
        pTrie->base  = 0;
        pTrie->check = 0;
        pTrie->node  = 0;
        pTrie->tail  = 0;
        pTrie->ntail = 0;
        pTrie->pos   = 0;

        // Allocate BASE & CHECK...
        n = init * sizeof( int ); // # bytes we'll need for each of
                                  // the two integer arrays
        pTrie->base = ( int* ) DATRIE_MALLOC( context, n );
        if ( ! pTrie->base ) break;

        pTrie->check = ( int* ) DATRIE_MALLOC( context, n );
        if ( ! pTrie->check ) break;

        // & initialize them:
        pTrie->base[ 1 ] = 0;     // We use zero to indicate an
                                  // unused slot,
        pTrie->check[ 0 ] = init; // and store the size of our
                                  // arrays in index zero.
        DATRIE_MEMSET( context, pTrie->base  + 2, 0,
                       sizeof( int ) * ( init - 2 ) );
        DATRIE_MEMSET( context, pTrie->check + 1, 0,
                       sizeof( int ) * ( init - 1 ) );

        // Next, we allocate NODE...
        n = init * sizeof( void* ); // # bytes we'll need for the
                                    // node array
        pTrie->node = ( tDaTrieNode* ) DATRIE_MALLOC( context, n );
        if ( ! pTrie->node ) break;

        // and initialize it; NODE[ i ] == 0 implies that there's no
        // value associated with node i in the tree.
        pTrie->node[ 0 ] = root;
        DATRIE_MEMSET( context, pTrie->node + 1, 0,
                       sizeof( tDaTrieNode ) * ( init - 1 ) );

        n = init;       // # bytes to allocate for string storage
        pTrie->tail = ( char* ) DATRIE_MALLOC( context, n );
        if ( ! pTrie->tail ) break;

        pTrie->ntail   = n;
        pTrie->pos     = 1;
        pTrie->context = context;

        *phTrie = pTrie;
        status  = eDaTrieStatus_Success;

    } while ( 0 );

    if ( pTrie && eDaTrieStatus_Success != status )
    {
        if ( pTrie->base  ) DATRIE_FREE( context, pTrie->base  );
        if ( pTrie->check ) DATRIE_FREE( context, pTrie->check );
        if ( pTrie->node  ) DATRIE_FREE( context, pTrie->node  );
        if ( pTrie->tail  ) DATRIE_FREE( context, pTrie->tail  );
        DATRIE_FREE( context, pTrie );
    }

    return status;

} // End datrieAllocate.

/// Free the memory associated with a previously allocated trie
void datrieFree( hDaTrie hTrie )
{
    ptDaTrie       pTrie;
    tDaTrieContext context;

    pTrie = hTrie;

    if ( pTrie )
    {
        context = pTrie->context;
        if ( pTrie->base  ) DATRIE_FREE( context, pTrie->base  );
        if ( pTrie->check ) DATRIE_FREE( context, pTrie->check );
        if ( pTrie->node  ) DATRIE_FREE( context, pTrie->node  );
        if ( pTrie->tail  ) DATRIE_FREE( context, pTrie->tail  );
        DATRIE_FREE( context, pTrie );
    }

} // End datrieFree.

/// Insert a new key/value pair into the trie
tDaTrieStatus datrieInsert( hDaTrie      hTrie,
                            const char  *key,
                            tDaTrieNode  value,
                            int          fReplace,
                            tDaTrieNode *pExtant )

{
    ptDaTrie      pTrie;
    size_t        nMatch, nLast;
    tDaTrieNode   pNode, pLast;
    tDaTrieStatus status;

    // Zero out our [out] parameter,
    if ( pExtant ) *pExtant = 0;
    // coerce the caller-supplied opaque handle to a properly typed
    // pointer,
    pTrie = hTrie;
    // & delegate to our core traversal algorithm:
    status = walk_trie( pTrie, key, value, 1, fReplace, &pNode, &nMatch,
                        &pLast, &nLast );

    if ( eDaTrieStatus_KeyAlreadyExists == status && pExtant )
    {
        *pExtant = pNode;
    }

    return status;

} // End datrieInsert.

/// Lookup a key/value pair in the trie (exact match)
tDaTrieStatus datrieMatch( hDaTrie      hTrie,
                           const char  *text,
                           tDaTrieNode *pValue )
{
    ptDaTrie      pTrie;
    size_t        nMatch, nLast;
    tDaTrieNode   pNode, pLast;
    tDaTrieStatus status;

    // Zero out our [out] parameter,
    if ( pValue ) *pValue = 0;
    // coerce the caller-supplied opaque handle to a properly typed
    // pointer,
    pTrie = hTrie;
    // & delegate to our core traversal algorithm:
    status = walk_trie( pTrie, text, 0, 0, 0, &pNode, &nMatch,
                        &pLast, &nLast );

    if ( eDaTrieStatus_Success == status && pValue )
    {
        *pValue = pNode;
    }

    return status;

} // End datrieMatch.

/// Lookup a key/value pair in the trie (longest prefix)
tDaTrieStatus datrieSearch( hDaTrie      hTrie,
                            const char  *text,
                            tDaTrieNode *pValue,
                            size_t      *pnMatch )
{
    ptDaTrie      pTrie;
    tDaTrieNode   pNode, pLast;
    tDaTrieStatus status;
    size_t        nMatch, nLast;

    // Zero out our [out] parameters,
    if ( pValue  ) *pValue  = 0;
    if ( pnMatch ) *pnMatch = 0;
    // coerce the caller-supplied opaque handle to a properly typed
    // pointer,
    pTrie = hTrie;
    // & delegate to our core traversal algorithm:
    status = walk_trie( pTrie, text, 0, 0, 0, &pNode, &nMatch,
                        &pLast, &nLast );

    if ( pValue  ) *pValue  = pLast;
    if ( pnMatch ) *pnMatch = nMatch;

    return nMatch ? eDaTrieStatus_Success : eDaTrieStatus_Failure;

} // End datrieSearch.

/// Visit all nodes in a trie, in no particular order
tDaTrieStatus datrieVisitNodes( hDaTrie          hTrie,
                                pfnDaTrieVisitor pfnVisitor )
{
    int            i;
    tDaTrieContext ctx;
    ptDaTrie       pTrie;

    // Ok-- coerce the caller-supplied opaque handle to a properly typed
    // pointer,
    pTrie = hTrie;
    // & get to work:
    ctx = pTrie->context;

    for ( i = 1; i < pTrie->check[ 0 ]; ++i )
    {
        if ( ! pfnVisitor( ctx, pTrie->node[ i ] ) ) break;
    }

    return eDaTrieStatus_Success;

} // End datrieVisitNodes.

#ifdef DATRIE_INCLUDE_DUMP_CODE

/// Dump a trie
void datrieDump( hDaTrie hTrie )
{
    ptDaTrie      pTrie;
    int           i, j;
    char          c;

    // Ok-- coerce the caller-supplied opaque handle to a properly typed
    // pointer,
    pTrie = hTrie;
    // & get to work:
    DATRIE_LOG1( pTrie->context, "A double-array trie at %p:\n", pTrie );
    DATRIE_LOG0( pTrie->context, "     BASE       CHECK      NODE\n" );
    for ( i = 0; i < pTrie->check[ 0 ]; ++i )
    {
        DATRIE_LOG4( pTrie->context, "%4d %-10d %-10d %p\n", i,
                     pTrie->base[ i ], pTrie->check[ i ],
                     pTrie->node[ i ] );
    }

    DATRIE_LOG0( pTrie->context, "\nTAIL:\n" );
    for ( i = 0; i < ( int ) pTrie->ntail / 50; ++i )
    {
        DATRIE_LOG0( pTrie->context,
                     "          1         2         3         4\n" );
        DATRIE_LOG0( pTrie->context,
                     "01234567890123456789012345678901234567890\n" );

        for ( j = i * 50; j < ( i + 1 ) * 50; ++j )
        {
            c = pTrie->tail[ j ];
            if ( ! pTrie->tail[ j ] ) c = '#';

            DATRIE_LOG1( pTrie->context, "%c", c );
        }

        DATRIE_LOG0( pTrie->context, "\n" );
    }

} // End datrieDump.

#endif // DATRIE_INCLUDE_DUMP_CODE


// Local Variables:
// fill-column: 72
// indent-tabs-mode: nil
// show-trailing-whitespace: t
// End:

// datrie.c ends here.
