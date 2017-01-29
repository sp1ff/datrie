/**
 * \file main.c
 *
 * \brief Driver program for the datrie module
 *
 *
 */

#include "datrie.h"

#include <stdlib.h>
#include <malloc.h>

#define false 0
#define true  1

/// Exercise datrie against Aoe's examples
void example( )
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
                ( tDaTrieNode  ) 2    == val    &&
                8                     == n );

        datrieFree( hTrie );
    }
}

/// Exercise datrie against a larger corpus
void gettysburg( )
{
    // Words in the Gettysburg address
    // (https://en.wikipedia.org/wiki/Gettysburg_Address#Text_of_Gettysburg_Address,
    // retrieved Januar 29, 2017).
    const char * const STRINGS[ ] = {
      "But", "Four", "God", "It", "Liberty", "Now", "The", "We", "a", "above"
      "add", "advanced", "ago", "all", "altogether", "and", "any", "are",
      "as", "battlefield", "be", "before", "birth", "brave", "brought", "but",
      "by", "can", "cause", "civil", "come", "conceived", "consecrate",
      "consecrated", "continent", "created", "dead", "dedicate", "dedicated",
      "detract", "devotion", "did", "died", "do", "earth", "endure", "engaged",
      "equal", "far", "fathers", "field", "final", "fitting", "for", "forget",
      "forth", "fought", "freedom", "from", "full", "gave", "government",
      "great", "ground", "hallow", "have", "here", "highly", "honored", "in",
      "increased", "is", "it", "larger", "last", "little", "live", "lives",
      "living", "long", "measure", "men", "met", "might", "nation", "never",
      "new", "nobly", "nor", "not", "note", "of", "on", "or", "our", "people",
      "perish", "place", "poor", "portion", "power", "proper", "proposition",
      "rather", "remaining", "remember", "resolve", "resting", "say", "score",
      "sense", "seven", "shall", "should", "so", "struggled", "take", "task",
      "testing", "that", "the", "their", "these", "they", "this", "those",
      "thus", "to", "under", "unfinished", "us", "vain", "war", "we", "what",
      "whether", "which", "who", "will", "work", "world", "years"
    };

    size_t        i;
    hDaTrie       hTrie;
    tDaTrieStatus status;
    tDaTrieNode   value;

    status = datrieAllocate( 0, 0, 0, &hTrie );
    if ( eDaTrieStatus_Success == status )
    {
        for ( i = 0; i < sizeof( STRINGS ) / sizeof( STRINGS[ 0 ] ); ++i )
        {
            status = datrieInsert( hTrie, STRINGS[ i ], ( tDaTrieNode ) 1,
                                   false, 0 );
            assert( eDaTrieStatus_Success == status );
        }

        for ( i = 0; i < sizeof( STRINGS ) / sizeof( STRINGS[ 0 ] ); ++i )
        {
            status = datrieMatch( hTrie, STRINGS[ i ], &value );
            assert( eDaTrieStatus_Success == status &&
                    ( tDaTrieNode ) 1 == value );
        }

        datrieFree( hTrie );
    }

} // End gettysburg.

int main( )
{
    printf("Testing datrie...");
    example( );
    gettysburg( );
    printf("done (success).\n");
    return 0;

} // End main.

// Local Variables:
// c-basic-offset: 4
// fill-column: 72
// indent-tabs-mode: nil
// show-trailing-whitespace: t
// End:
