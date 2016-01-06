| package |
package := Package name: 'CU Sortblocks'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2001-2005.
chris.uppal@metagnostic.org

A few objects that provide easier ways of creating the <diadicValuables> for SortedCollection.  The main advantage is improved readability, however it also
can improve performance in some cases (see the SortBlock class comment).

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris


History:

3.00
-	Added  a sensible #printOn: method.
-	Since the ''CU Object Methods'' package is now public, the #beOptimized (etc) methods can
	now be used, so I''ve added some explanatory comments.

2.00
-	Removed Symbol>>value: which I''d left in by mistake (it was needed by an early version).

1.00
-	First release.
'.

package basicPackageVersion: '3.01'.


package classNames
	add: #SortAscending;
	add: #SortAscendingWithNils;
	add: #SortBlock;
	add: #SortDescending;
	add: #SortDescendingWithNils;
	add: #SortStringsAscending;
	add: #SortStringsDescending;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'CU String Extensions';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Base\Dolphin';
	yourself).

package setManualPrerequisites: #(
	'CU String Extensions').

package!

"Class Definitions"!

Object subclass: #SortBlock
	instanceVariableNames: 'selector cache'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
SortBlock subclass: #SortAscending
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
SortBlock subclass: #SortDescending
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
SortBlock subclass: #SortStringsAscending
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
SortBlock subclass: #SortStringsDescending
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
SortAscending subclass: #SortAscendingWithNils
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
SortDescending subclass: #SortDescendingWithNils
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

SortBlock guid: (GUID fromString: '{E20542D5-53C3-11D3-8725-BC9EBD3E4405}')!
SortBlock comment: 'Copyright © Chris Uppal, 2001-2005.
chris.uppal@metagnostic.org

Class of objects that can be used instead of BlockClosures as the <dyadicValuable> of (e.g.) a SortedCollection.  This class is itself abstract, the four subclasses deal with sorting up/down, and there are two special cases for sorting strings.

Actually all the four subclasses can be used as sort blocks themselves, e.g:

	... asSortedCollection: SortStringsDescending.

or instances can be created which do the same thing more flexibly.  You can create an instance which will sort on the result of sending a message (expresded as a Symbol) to the sorted elements, e.g:

	... asSortedCollection: (SortAscending by: #name).

which I think is clearer than messing with explicit BlockClosures.

You can ask the sort block to maintain a cache of the results of that block, which is very useful in the (relatively rare, but not nearly rare enough) cases where that computation is a significant part of the cost of the comparison, e.g:

	Class allClasses asSortedCollection: (SortAscending caching: #definitionChunk).

(OK, that''s a wierd example.)

The cache uses a WeakIdentityDictionary with weak keys.
'!
!SortBlock categoriesForClass!No category! !
!SortBlock methodsFor!

beCached
	"private -- tell the receiver to use a cache to hold of the results of evaluating
	its block"

	cache isNil ifTrue:
		[cache := self newCache.
		#CUtodo. "self beNotOptimized"].!

beNotCached
	"private -- tell the receiver not to use a cache of the results of evaluating its block"

	self isCached ifTrue:
		[cache := nil.
		#CUtodo. "self beNotOptimized"].!

beNotOptimized
	"experimental -- ensure that we are (no longer) 'optimised'.
	See the comment in #beOptimized for more explanation"

	self isOptimized ifTrue: [self removeSpecializedSelector: #value:value:].
	!

beOptimized
	"experimental -- ensure that we are 'optimised'.
	That means that instead of using #perform: at runtime to evaluate
	our selector, we generate an object-specific implementation of #value:value:
	in which the message send is hard-coded.
	NB: this will only work if #addSpecializedMethod: is defined.  The intended
	implementation is in the 'CU Object Methods' package, but please note that
	there is no formal dependency from this package to that (because this is
	only an experimental feature), so if you *do* use this optimisation in a deployed
	application then you'll have to ensure that object methods package is not
	stripped (and you'll have to ship the compiler DLL with the app)"

	self addSpecializedMethod: self valueValueMethodString.
	!

cacheStringPost
	"experimental!!"

	^ self isCached
		ifTrue: [']']
		ifFalse: [''].
	!

cacheStringPreFor: aName
	"experimental!!"

	^ self isCached
		ifTrue: ['cache at: ' , aName , ' ifAbsentPut: [']
		ifFalse: [''].
	!

comparisonStringOp
	"experimental!!"

	#subclassResponsibility.
	^ ')
		value: ('.!

comparisonStringPost
	"experimental!!"

	#subclassResponsibility.
	^ ')'.
!

comparisonStringPre
	"experimental!!"

	#subclassResponsibility.
	^ 'self class
		value: ('.
!

evaluationStringFor: aName
	"experimental!!"

	^ aName , ' ' , selector.!

flushCache
	"discard any cached data in the receiver"

	self isCached ifTrue: [cache := self newCache].!

isCached
	"answer whether the receiver caches the results of evaluating its block"

	^ cache notNil.!

isOptimized
	"experimental -- answer whether we are 'optimised'.
	See the comment in #beOptimized for more explanation"

	^ self isSpecializedOn: #value:value:.!

newCache
	"private -- answer a new cache suitable for holding the results of
	evaluating our block"

	^ (WeakIdentityDictionary new)
		haveWeakKeys;
		haveStrongValues;
		yourself.
!

printOn: aStream
	"append a developer friendly representation of ourself to the given stream.
	This implementation allows the reciever to be recreated (e.g. in an Inspector).
	Thanks to Martin Rubi for the suggestion."

	aStream
		display: self class;
		nextPutAll: (self isCached ifTrue: [' caching: '] ifFalse: [' by: ']);
		print: selector.!

selector: aSymbol
	"private -- set the <monadicValuable> to sort by"

	selector := aSymbol.
	self flushCache.
	#CUtodo. "self beNotOptimized."!

value: anObject1 value: anObject2
	"implement this part of the <dyadicValuable> protocol in order that
	the receiver can be a sort block"

	^ cache isNil
		ifTrue: [self class
				value: (anObject1 perform: selector)
				value: (anObject2 perform: selector)]
		ifFalse: [self class
				value: (cache at: anObject1 ifAbsentPut: [anObject1 perform: selector])
				value: (cache at: anObject2 ifAbsentPut: [anObject2 perform: selector])].
!

valueValueMethodString
	"private -- answer the text of a specialised #value:value method"

	^ (String writeStream)
		nextPutAll: 'value: obj1 value: obj2';
		crtab;
		nextPutAll: '^ ';
		nextPutAll: self comparisonStringPre;
		nextPutAll: (self cacheStringPreFor: 'obj1');
		nextPutAll: (self evaluationStringFor: 'obj1');
		nextPutAll: self cacheStringPost;
		nextPutAll: self comparisonStringOp;
		nextPutAll: (self cacheStringPreFor: 'obj2');
		nextPutAll: (self evaluationStringFor: 'obj2');
		nextPutAll: self cacheStringPost;
		nextPutAll: self comparisonStringPost;
		nextPutAll: '.';
		contents.
! !
!SortBlock categoriesFor: #beCached!initializing!private! !
!SortBlock categoriesFor: #beNotCached!initializing!private! !
!SortBlock categoriesFor: #beNotOptimized!optimizing!public! !
!SortBlock categoriesFor: #beOptimized!optimizing!public! !
!SortBlock categoriesFor: #cacheStringPost!helpers!private! !
!SortBlock categoriesFor: #cacheStringPreFor:!helpers!private! !
!SortBlock categoriesFor: #comparisonStringOp!helpers!private! !
!SortBlock categoriesFor: #comparisonStringPost!helpers!private! !
!SortBlock categoriesFor: #comparisonStringPre!helpers!private! !
!SortBlock categoriesFor: #evaluationStringFor:!helpers!private! !
!SortBlock categoriesFor: #flushCache!operations!public! !
!SortBlock categoriesFor: #isCached!public!testing! !
!SortBlock categoriesFor: #isOptimized!optimizing!public!testing! !
!SortBlock categoriesFor: #newCache!helpers!private! !
!SortBlock categoriesFor: #printOn:!printing!public! !
!SortBlock categoriesFor: #selector:!initializing!private! !
!SortBlock categoriesFor: #value:value:!evaluating!public! !
!SortBlock categoriesFor: #valueValueMethodString!helpers!private! !

!SortBlock class methodsFor!

by: aSymbol
	"answer an instance which will sort by the result of #perform:ing
	the given Symbol"

	^ (self new)
		selector: aSymbol;
		yourself.!

cacheing: aSymbol
	"answer an instance which will sort by the result of result of #perform:ing
	the given Symbol, and will also keep the results in a cache so as to
	reduce execution time for sorting by slow derived computations"

	^ (self by: aSymbol)
		beCached;
		yourself.!

caching: aSymbol
	"answer an instance which will sort by the result of result of #perform:ing
	the given Symbol, and will also keep the results in a cache so as to
	reduce execution time for sorting by slow derived computations"

	"how *is* cacheing supposed to be spelled ?"

	^ self cacheing: aSymbol.!

value: anObject1 value: anObject2
	"implement this part of the <dyadicValuable> protocol in order that
	the receiver can be a sort 'block'"

	self subclassResponsibility.! !
!SortBlock class categoriesFor: #by:!instance creation!public! !
!SortBlock class categoriesFor: #cacheing:!instance creation!public! !
!SortBlock class categoriesFor: #caching:!instance creation!public! !
!SortBlock class categoriesFor: #value:value:!evaluating!public! !

SortAscending guid: (GUID fromString: '{E20542DC-53C3-11D3-8725-BC9EBD3E4405}')!
SortAscending comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org'!
!SortAscending categoriesForClass!No category! !
!SortAscending methodsFor!

comparisonStringOp
	"experimental!!"

	^ ') < ('.!

comparisonStringPost
	"experimental!!"

	^ ')'.
!

comparisonStringPre
	"experimental!!"

	^ '('.
! !
!SortAscending categoriesFor: #comparisonStringOp!helpers!private! !
!SortAscending categoriesFor: #comparisonStringPost!helpers!private! !
!SortAscending categoriesFor: #comparisonStringPre!helpers!private! !

!SortAscending class methodsFor!

value: anObject1 value: anObject2
	"implement this part of the <dyadicValuable> protocol in order that
	the receiver can be a sort 'block'"

	^ anObject1 <= anObject2.! !
!SortAscending class categoriesFor: #value:value:!evaluating!public! !

SortDescending guid: (GUID fromString: '{E20542DB-53C3-11D3-8725-BC9EBD3E4405}')!
SortDescending comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org'!
!SortDescending categoriesForClass!No category! !
!SortDescending methodsFor!

comparisonStringOp
	"experimental!!"

	^ ') > ('.!

comparisonStringPost
	"experimental!!"

	^ ')'.
!

comparisonStringPre
	"experimental!!"

	^ '('.
! !
!SortDescending categoriesFor: #comparisonStringOp!helpers!private! !
!SortDescending categoriesFor: #comparisonStringPost!helpers!private! !
!SortDescending categoriesFor: #comparisonStringPre!helpers!private! !

!SortDescending class methodsFor!

value: anObject1 value: anObject2
	"implement this part of the <dyadicValuable> protocol in order that
	the receiver can be a sort 'block'"

	^ anObject2 <= anObject1.! !
!SortDescending class categoriesFor: #value:value:!evaluating!public! !

SortStringsAscending guid: (GUID fromString: '{791BFD41-830C-11D3-8725-C494F4CEBD02}')!
SortStringsAscending comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Sort Strings in increasing order using the ''C'' locale, case sensitive.'!
!SortStringsAscending categoriesForClass!Unclassified! !
!SortStringsAscending methodsFor!

comparisonStringOp
	"experimental!!"

	^ ') strcmp: ('.!

comparisonStringPost
	"experimental!!"

	^ ')) < 0'.
!

comparisonStringPre
	"experimental!!"

	^ '(('.
! !
!SortStringsAscending categoriesFor: #comparisonStringOp!helpers!private! !
!SortStringsAscending categoriesFor: #comparisonStringPost!helpers!private! !
!SortStringsAscending categoriesFor: #comparisonStringPre!helpers!private! !

!SortStringsAscending class methodsFor!

value: aString1 value: aString2
	"implement this part of the <dyadicValuable> protocol in order that
	the receiver can be a sort 'block' which improves sorting
	performance"

	^ (aString1 strcmp: aString2) <= 0.
! !
!SortStringsAscending class categoriesFor: #value:value:!evaluating!public! !

SortStringsDescending guid: (GUID fromString: '{791BFD40-830C-11D3-8725-C494F4CEBD02}')!
SortStringsDescending comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Sort Strings in decreasing order using the ''C'' locale, case sensitive.'!
!SortStringsDescending categoriesForClass!Unclassified! !
!SortStringsDescending methodsFor!

comparisonStringOp
	"experimental!!"

	^ ') strcmp: ('.!

comparisonStringPost
	"experimental!!"

	^ ')) > 0'.
!

comparisonStringPre
	"experimental!!"

	^ '(('.
! !
!SortStringsDescending categoriesFor: #comparisonStringOp!helpers!private! !
!SortStringsDescending categoriesFor: #comparisonStringPost!helpers!private! !
!SortStringsDescending categoriesFor: #comparisonStringPre!helpers!private! !

!SortStringsDescending class methodsFor!

value: aString1 value: aString2
	"implement this part of the <dyadicValuable> protocol in order that
	the receiver can be a sort 'block' which improves sorting
	performance"

	^ (aString1 strcmp: aString2) >= 0.
! !
!SortStringsDescending class categoriesFor: #value:value:!evaluating!public! !

SortAscendingWithNils guid: (GUID fromString: '{3F99F7C8-ABFE-4908-A5F9-BE4240EEF9CF}')!
SortAscendingWithNils comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org'!
!SortAscendingWithNils categoriesForClass!Unclassified! !
!SortAscendingWithNils class methodsFor!

value: anObject1 value: anObject2
	"implement this part of the <dyadicValuable> protocol in order that
	the receiver can be a sort 'block'"

	"we consider nil to be <= every other object"
	anObject1 isNil ifTrue: [^ true].
	anObject2 isNil ifTrue: [^ false].

	^ super value: anObject1 value: anObject2.! !
!SortAscendingWithNils class categoriesFor: #value:value:!evaluating!public! !

SortDescendingWithNils guid: (GUID fromString: '{4AFD8AD7-2E17-4B70-80A6-099BD3C3EBDF}')!
SortDescendingWithNils comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org'!
!SortDescendingWithNils categoriesForClass!Unclassified! !
!SortDescendingWithNils class methodsFor!

value: anObject1 value: anObject2
	"implement this part of the <dyadicValuable> protocol in order that
	the receiver can be a sort 'block'"

	"we consider nil to be <= every other object (except nil)"
	anObject1 isNil ifTrue: [^ false].
	anObject2 isNil ifTrue: [^ true].

	^ super value: anObject1 value: anObject2.
! !
!SortDescendingWithNils class categoriesFor: #value:value:!evaluating!public! !

"Binary Globals"!

"Resources"!

