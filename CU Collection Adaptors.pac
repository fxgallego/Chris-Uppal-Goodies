| package |
package := Package name: 'CU Collection Adaptors'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2002.
chris.uppal@metagnostic.org

Builds on the Abstract Collections package to provide a number of simple adaptor classes.  Adaptors wrap some exising object that implements a small part of the interface of some collection/stream, and extends it to support the entire interface.  Note that this is really just a kludge to get around the absence of, say, mixins in Smalltalk.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris'.

package basicPackageVersion: '1.02'.


package classNames
	add: #ArrayAdaptor;
	add: #CollectionAdaptor;
	add: #DictionaryAdaptor;
	add: #OrderedCollectionAdaptor;
	add: #PositionableReadStreamAdaptor;
	add: #PositionableWriteStreamAdaptor;
	add: #ReadStreamAdaptor;
	add: #SetAdaptor;
	add: #StreamAdaptor;
	add: #WriteStreamAdaptor;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'CU Abstract Collections';
	yourself).

package!

"Class Definitions"!

AbstractCollection subclass: #CollectionAdaptor
	instanceVariableNames: 'subject diyIncludes'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
AbstractArray subclass: #ArrayAdaptor
	instanceVariableNames: 'subject diyLoop diyReplace'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
AbstractOrderedCollection subclass: #OrderedCollectionAdaptor
	instanceVariableNames: 'subject diyLoop diyClear diyReplace'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
AbstractSet variableSubclass: #SetAdaptor
	instanceVariableNames: 'subject'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
AbstractDictionary variableSubclass: #DictionaryAdaptor
	instanceVariableNames: 'subject diyIncludes'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
AbstractReadStream subclass: #ReadStreamAdaptor
	instanceVariableNames: 'subject diyNextInto'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ReadStreamAdaptor subclass: #PositionableReadStreamAdaptor
	instanceVariableNames: 'diyAtEnd'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
AbstractWriteStream subclass: #WriteStreamAdaptor
	instanceVariableNames: 'subject diyNextPut diyNextPutAll'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
AbstractReadWriteStream subclass: #StreamAdaptor
	instanceVariableNames: 'subject diyNextInto diyNextPut diyNextPutAll diyAtEnd'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
WriteStreamAdaptor subclass: #PositionableWriteStreamAdaptor
	instanceVariableNames: 'diyAtEnd'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

CollectionAdaptor guid: (GUID fromString: '{DD45BF56-7A30-47F7-B6CF-F0D599786C3F}')!
CollectionAdaptor comment: 'Copyright © Chris Uppal, 2002.
chris.uppal@metagnostic.org

Adaptor to make any subject which understands <#add:, #do: #remove:ifAbsent: #size> and (optionally) #includes: function as a Collection.'!
!CollectionAdaptor categoriesForClass!Unclassified! !
!CollectionAdaptor methodsFor!

add: anObject
	"one of the root methods for <Collection>"

	^ subject add: anObject.
!

do: a1Block
	"one of the root methods for <Collection>"

	^ subject do: a1Block.!

includes: anObject
	"one of the root methods of Collection; optional because it can be implemented
	in terms of #do:"

	^ diyIncludes
		ifTrue: [super includes: anObject]
		ifFalse: [subject includes: anObject].
!

remove: anObject ifAbsent: a0Block
	"one of the root methods for <Collection>"

	^ subject remove: anObject ifAbsent: a0Block.
!

size
	"one of the root methods for <Collection>"

	^ subject size.!

subject
	"private -- answer the receiver's subject"

	^ subject.
!

subject: anObject
	"private -- set the receiver's subject to anObject"

	subject := anObject.
	diyIncludes := (subject respondsTo: #includes) not.! !
!CollectionAdaptor categoriesFor: #add:!adding!public! !
!CollectionAdaptor categoriesFor: #do:!enumerating!public! !
!CollectionAdaptor categoriesFor: #includes:!public!searching! !
!CollectionAdaptor categoriesFor: #remove:ifAbsent:!public!removing! !
!CollectionAdaptor categoriesFor: #size!accessing!public! !
!CollectionAdaptor categoriesFor: #subject!accessing!public! !
!CollectionAdaptor categoriesFor: #subject:!initializing!private! !

!CollectionAdaptor class methodsFor!

for: anObject
	"answer a new instance which wraps anObject and forwards #add:, #do: #remove:ifAbsent:
	and #size to that object.  If the subject doesn't understand #species or responds to it with its
	own class (which can't be suitable for our purposes or it wouldn't be wrapped in one of these)
	then we'll ask for it's species when copying.  If the subject also understands #includes: then
	that's gravy and we'll forward it too"

	^ (self basicNew: 1)		"using a non-zero basic size may help avoid divide-by-zero errors"
		subject: anObject;
		yourself.! !
!CollectionAdaptor class categoriesFor: #for:!instance creation!public! !

ArrayAdaptor guid: (GUID fromString: '{F2A8E3DF-FCB0-4608-901A-93B05022F336}')!
ArrayAdaptor comment: 'Copyright © Chris Uppal, 2002.
chris.uppal@metagnostic.org

Adaptor to make any subject which understands <#size, #at: #at:put:> and (optionally) <#from:to:keysAndValuesDo: #replaceFrom:to:with:startingAt:>, function as a SequenceableCollection.'!
!ArrayAdaptor categoriesForClass!Unclassified! !
!ArrayAdaptor methodsFor!

at: anInteger
	"one of the root methods for <SequenceableCollection>"

	^ subject at: anInteger.!

at: anInteger put: anObject
	"one of the root methods for <SequenceableCollection>"

	^ subject at: anInteger put: anObject.!

from: aStartIndex to: aStopIndex keysAndValuesDo: a2Block
	"one of the root methods for <SequenceableCollection>, optional because it can be
	implemented in terms of #at: and #size"

	"if the subject doesn't understand this, then we inline the loop here for speed rather then
	falling back to the superclass implementation"
	^ diyLoop
		ifTrue: [aStartIndex to: aStopIndex do: [:i | a2Block value: i value: (subject at: i)]]
		ifFalse: [subject from: aStartIndex to: aStopIndex keysAndValuesDo: a2Block].!

replaceFrom: aStartIndex to: aStopIndex with: replacementElements startingAt: aReplacementIndex
	"one of the root methods for <SequenceableCollection>, optional because it can be
	implemented in terms of #at:, #at:put:, and #size"

	^ diyReplace
		ifTrue: [super replaceFrom: aStartIndex to: aStopIndex with: replacementElements startingAt: aReplacementIndex]
		ifFalse: [subject replaceFrom: aStartIndex to: aStopIndex with: replacementElements startingAt: aReplacementIndex].
!

size
	"one of the root methods for <Collection>"

	^ subject size.!

subject
	"private -- answer the receiver's subject"

	^ subject.
!

subject: anObject
	"private -- set the receiver's subject to anObject"

	subject := anObject.

	diyLoop := (anObject respondsTo: #from:to:keysAndValuesDo:) not.
	diyReplace := (anObject respondsTo: #replaceFrom:to:with:startingAt:) not.
! !
!ArrayAdaptor categoriesFor: #at:!accessing!public! !
!ArrayAdaptor categoriesFor: #at:put:!accessing!public! !
!ArrayAdaptor categoriesFor: #from:to:keysAndValuesDo:!enumerating!public! !
!ArrayAdaptor categoriesFor: #replaceFrom:to:with:startingAt:!public!replacing! !
!ArrayAdaptor categoriesFor: #size!accessing!public! !
!ArrayAdaptor categoriesFor: #subject!accessing!public! !
!ArrayAdaptor categoriesFor: #subject:!initializing!private! !

!ArrayAdaptor class methodsFor!

for: anObject
	"answer a new instance which wraps anObject and forwards #size, #at: and #at:put:
	to that object;  if the subject also understands #from:to:keysAndValuesDo: or, #replaceFrom:to:with:startingAt:
	then that's gravy and we'll forward them too"

	^ (self basicNew)
		subject: anObject;
		yourself.! !
!ArrayAdaptor class categoriesFor: #for:!instance creation!public! !

OrderedCollectionAdaptor guid: (GUID fromString: '{CB4D3C3C-5C2E-400F-AA4B-3E583F62C61B}')!
OrderedCollectionAdaptor comment: 'Copyright © Chris Uppal, 2002.
chris.uppal@metagnostic.org

Adaptor to make any subject which understands <#size, #at: #at:put: #add:afterIndex: #removeAtIndex:> and (optionally) <#from:to:keysAndValuesDo: #removeAll #replaceFrom:to:with:startingAt:>, function as a SequencedGrowableCollection.'!
!OrderedCollectionAdaptor categoriesForClass!Unclassified! !
!OrderedCollectionAdaptor methodsFor!

add: anObject afterIndex: anIndex
	"one of the root methods for <SequencedGrowableCollection>"

	^ subject add: anObject afterIndex: anIndex.!

at: anIndex
	"one of the root methods for <SequenceableCollection>"

	^ subject at: anIndex.!

at: anIndex put: anObject
	"one of the root methods for <SequenceableCollection>"

	^ subject at: anIndex put: anObject.!

from: aStartIndex to: aStopIndex keysAndValuesDo: a2Block
	"one of the root methods for <SequenceableCollection>, optional because it can be
	implemented in terms of #at: and #size"

	"if the subject doesn't understand this, then we inline the loop here for speed rather then
	falling back to the superclass implementation"
	^ diyLoop
		ifTrue: [aStartIndex to: aStopIndex do: [:i | a2Block value: i value: (subject at: i)]]
		ifFalse: [subject from: aStartIndex to: aStopIndex keysAndValuesDo: a2Block].!

removeAll
	"one of the root methods for <SequencedGrowableCollection>, optional because it can be
	implemented in terms of #removeAtIndex: and #size"

	diyClear
		ifTrue: [super removeAll]
		ifFalse: [subject removeAll].!

removeAtIndex: anIndex
	"one of the root methods for <SequencedGrowableCollection>"

	^ subject removeAtIndex: anIndex.
!

replaceFrom: aStartIndex to: aStopIndex with: replacementElements startingAt: aReplacementIndex
	"one of the root methods for <SequenceableCollection>, optional because it can be
	implemented in terms of #at:, #at:put:, and #size"

	^ diyReplace
		ifTrue: [super replaceFrom: aStartIndex to: aStopIndex with: replacementElements startingAt: aReplacementIndex]
		ifFalse: [subject replaceFrom: aStartIndex to: aStopIndex with: replacementElements startingAt: aReplacementIndex].
!

size
	"one of the root methods for <Collection>"

	^ subject size.!

subject
	"private -- answer the receiver's subject"

	^ subject.
!

subject: anObject
	"private -- set the receiver's subject to anObject"

	subject := anObject.

	diyLoop := (anObject respondsTo: #from:to:keysAndValuesDo:) not.
	diyClear := (anObject respondsTo: #removeAll) not.
	diyReplace := (anObject respondsTo: #replaceFrom:to:with:startingAt:) not.
! !
!OrderedCollectionAdaptor categoriesFor: #add:afterIndex:!adding!public! !
!OrderedCollectionAdaptor categoriesFor: #at:!accessing!public! !
!OrderedCollectionAdaptor categoriesFor: #at:put:!accessing!public! !
!OrderedCollectionAdaptor categoriesFor: #from:to:keysAndValuesDo:!enumerating!public! !
!OrderedCollectionAdaptor categoriesFor: #removeAll!public!removing! !
!OrderedCollectionAdaptor categoriesFor: #removeAtIndex:!public!removing! !
!OrderedCollectionAdaptor categoriesFor: #replaceFrom:to:with:startingAt:!public!replacing! !
!OrderedCollectionAdaptor categoriesFor: #size!accessing!public! !
!OrderedCollectionAdaptor categoriesFor: #subject!accessing!public! !
!OrderedCollectionAdaptor categoriesFor: #subject:!initializing!private! !

!OrderedCollectionAdaptor class methodsFor!

for: anObject
	"answer a new instance which wraps anObject and forwards #size, #at: #at:put: #add:afterIndex:
	and #removeAtIndex: to that object;  if the subject also understands #from:to:keysAndValuesDo:,
	#removeAll, or #replaceFrom:to:with:startingAt: then that's gravy and we'll forward them too"

	^ (self basicNew)
		subject: anObject;
		yourself.! !
!OrderedCollectionAdaptor class categoriesFor: #for:!instance creation!public! !

SetAdaptor guid: (GUID fromString: '{A997A5C8-FE57-4576-A11C-09192E214BDD}')!
SetAdaptor comment: 'Copyright © Chris Uppal, 2002.
chris.uppal@metagnostic.org

Adaptor to make any subject which understands <#add:, #do: #remove:ifAbsent: #size #includes> function as a Set.'!
!SetAdaptor categoriesForClass!Unclassified! !
!SetAdaptor methodsFor!

add: anObject
	"one of the root methods for <Collection>"

	^ subject add: anObject.
!

do: a1Block
	"one of the root methods for <Collection>"

	^ subject do: a1Block.!

includes: anObject
	"one of the root methods for <Set>"

	^ subject includes: anObject.
!

rehash
	"not sure what's the best way to try to handle this"

	(subject respondsTo: #rehash) ifTrue: [subject rehash].!

remove: anObject ifAbsent: a0Block
	"one of the root methods for <Collection>"

	^ subject remove: anObject ifAbsent: a0Block.
!

shrink
	"not sure what's the best way to try to handle this"

	(subject respondsTo: #shrink) ifTrue: [subject shrink].!

size
	"one of the root methods for <Collection>"

	^ subject size.!

subject
	"private -- answer the receiver's subject"

	^ subject.
!

subject: anObject
	"private -- set the receiver's subject to anObject"

	subject := anObject.! !
!SetAdaptor categoriesFor: #add:!adding!public! !
!SetAdaptor categoriesFor: #do:!enumerating!public! !
!SetAdaptor categoriesFor: #includes:!public!searching! !
!SetAdaptor categoriesFor: #rehash!operations!public! !
!SetAdaptor categoriesFor: #remove:ifAbsent:!public!removing! !
!SetAdaptor categoriesFor: #shrink!mutating!public! !
!SetAdaptor categoriesFor: #size!accessing!public! !
!SetAdaptor categoriesFor: #subject!accessing!public! !
!SetAdaptor categoriesFor: #subject:!initializing!private! !

!SetAdaptor class methodsFor!

for: anObject
	"answer a new instance which wraps anObject and forwards #add:, #do: #remove:ifAbsent:
	#includes, and #size to that object.  If the subject understands #rehash and #shrink then they
	are also forwarded"

	^ (self basicNew: 1)		"using a capacity of 1 may help avoid divide-by-zero errors"
		subject: anObject;
		yourself.! !
!SetAdaptor class categoriesFor: #for:!instance creation!public! !

DictionaryAdaptor guid: (GUID fromString: '{5B457856-C1FA-4C10-86E0-3561053C41EC}')!
DictionaryAdaptor comment: 'Copyright © Chris Uppal, 2002.
chris.uppal@metagnostic.org

Adaptor to make any subject which understands <#at:ifAbsent #at:put: #keysAndValuesDo: #removeKey:ifAbsent: #size> and, optionaly <#includesKey:> function as a Dictionary.'!
!DictionaryAdaptor categoriesForClass!Unclassified! !
!DictionaryAdaptor methodsFor!

at: aKey ifAbsent: a0Block
	"one of the root methods for <Dictionary>"

	^ subject at: aKey ifAbsent: a0Block.!

at: aKey put: anObject
	"one of the root methods for <Dictionary>"

	^ subject at: aKey put: anObject.
!

includesKey: aKey
	"one of the root methods for <Dictionary>, optional because implementable in terms
	of at:ifAbsent:"

	^ diyIncludes
		ifTrue: [subject at: aKey ifAbsent: [^ false]. true]
		ifFalse: [subject includesKey: aKey].!

keysAndValuesDo: a2Block
	"one of the root methods for <Dictionary>"

	^ subject keysAndValuesDo: a2Block.!

rehash
	"not sure what's the best way to try to handle this"

	(subject respondsTo: #rehash) ifTrue: [subject rehash].!

removeKey: aKey ifAbsent: a0Block
	"one of the root methods for <Dictionary>"

	^ subject removeKey: aKey ifAbsent: a0Block.!

shrink
	"not sure what's the best way to try to handle this"

	(subject respondsTo: #shrink) ifTrue: [subject shrink].!

size
	"one of the root methods for <Collection>"

	^ subject size.!

subject
	"private -- answer the receiver's subject"

	^ subject.
!

subject: anObject
	"private -- set the receiver's subject to anObject"

	subject := anObject.

	diyIncludes := (anObject respondsTo: #includesKey:) not.
! !
!DictionaryAdaptor categoriesFor: #at:ifAbsent:!accessing!public! !
!DictionaryAdaptor categoriesFor: #at:put:!accessing!public! !
!DictionaryAdaptor categoriesFor: #includesKey:!public!searching! !
!DictionaryAdaptor categoriesFor: #keysAndValuesDo:!enumerating!public! !
!DictionaryAdaptor categoriesFor: #rehash!operations!public! !
!DictionaryAdaptor categoriesFor: #removeKey:ifAbsent:!public!removing! !
!DictionaryAdaptor categoriesFor: #shrink!mutating!public! !
!DictionaryAdaptor categoriesFor: #size!accessing!public! !
!DictionaryAdaptor categoriesFor: #subject!accessing!public! !
!DictionaryAdaptor categoriesFor: #subject:!initializing!private! !

!DictionaryAdaptor class methodsFor!

for: anObject
	"answer a new instance which wraps anObject and forwards #at:ifAbsent, #at:put:, #keysAndValuesDo:,
	#removeKey:ifAbsent:, and #size to that object.  If the subject understands #includesKey:, #rehash, or
	#shrink then they are also forwarded"

	^ (self basicNew: 1)		"using a capacity of 1 may help avoid divide-by-zero errors"
		subject: anObject;
		yourself.! !
!DictionaryAdaptor class categoriesFor: #for:!instance creation!public! !

ReadStreamAdaptor guid: (GUID fromString: '{5E5BF07E-BF92-4C2F-968E-703AE845280E}')!
ReadStreamAdaptor comment: 'Copyright © Chris Uppal, 2002.
chris.uppal@metagnostic.org

Adaptor to make any subject which understands <#atEnd #next> function as a readable stream; if it also understands #do:, #next:into:startingAt:, or #contents then it also forwards those.

It doesn''t forward <#size #position #position:> and so won''t act as a positionable stream (we have #shouldNotImplement for these). See PositionableReadStreamAdaptor.'!
!ReadStreamAdaptor categoriesForClass!Unclassified! !
!ReadStreamAdaptor methodsFor!

atEnd
	"one of the root methods for <ReadStream>"

	^ subject atEnd.!

close
	"one of the root methods for <ReadStream>"

	(subject respondsTo: #close) ifTrue: [subject close].
	subject := nil.!

contents
	"if our subject understands #contents then answer the result of forwarding to it,
	otherwise throw an error"

	^ (subject respondsTo: #contents)
		ifTrue: [subject contents]
		ifFalse: [super contents].!

do: a1Block
	"if our subject understands #do: then answer the result of forwarding to it,
	otherwise execure the loop ourselves"

	^ (subject respondsTo: #do)
		ifTrue: [subject do: a1Block]
		ifFalse: [super do: a1Block].
!

next
	"one of the root methods for <ReadStream>"

	^ subject next.!

next: anInteger into: aSequenceableCollection startingAt: anIndex
	"one of the root methods for <ReadStream>; optional because implementable in terms
	of #next"

	^ diyNextInto
		ifTrue: [super next: anInteger into: aSequenceableCollection startingAt: anIndex]
		ifFalse: [subject next: anInteger into: aSequenceableCollection startingAt: anIndex].
!

position
	"one of the root methods for positionable Streams, which we aren't, hence it's
	forbidden"

	self shouldNotImplement.
!

position: anInteger
	"one of the root methods for positionable Streams, which we aren't, hence it's
	forbidden"

	self shouldNotImplement.!

size
	"one of the root methods for positionable Streams, which we aren't, hence it's
	forbidden"

	self shouldNotImplement.!

subject: anObject
	"private -- set the object to which we forward the basic stream messages"

	subject := anObject.

	diyNextInto := (anObject respondsTo: #next:into:startingAt:) not.
! !
!ReadStreamAdaptor categoriesFor: #atEnd!public!streaming!testing! !
!ReadStreamAdaptor categoriesFor: #close!operations!public! !
!ReadStreamAdaptor categoriesFor: #contents!accessing!public! !
!ReadStreamAdaptor categoriesFor: #do:!enumerating!public!streaming! !
!ReadStreamAdaptor categoriesFor: #next!accessing!enumerating!public!streaming! !
!ReadStreamAdaptor categoriesFor: #next:into:startingAt:!accessing!public! !
!ReadStreamAdaptor categoriesFor: #position!positioning!public! !
!ReadStreamAdaptor categoriesFor: #position:!positioning!public! !
!ReadStreamAdaptor categoriesFor: #size!accessing!public! !
!ReadStreamAdaptor categoriesFor: #subject:!initializing!private! !

!ReadStreamAdaptor class methodsFor!

for: anObject
	"answer a new instance which wraps anObject and forwards #atEnd: and #next: to it.  If it
	also understands #do:, #next:into:startingAt:, or #contents then they are also forwarded"

	^ (self basicNew)
		subject: anObject;
		yourself.! !
!ReadStreamAdaptor class categoriesFor: #for:!instance creation!public! !

PositionableReadStreamAdaptor guid: (GUID fromString: '{090DA829-EE09-48AB-A416-EC45DEBE2C5A}')!
PositionableReadStreamAdaptor comment: 'Copyright © Chris Uppal, 2002.
chris.uppal@metagnostic.org

Adaptor which is the same as ReadStreamAdaptor except that it also forwards <#size #position #position:>, and provides a default implementation of #atEnd.'!
!PositionableReadStreamAdaptor categoriesForClass!Unclassified! !
!PositionableReadStreamAdaptor methodsFor!

atEnd
	"overridden from the superclass definition so that we can provide a fallback
	implementation (which, however, may be slow) in case the subject can't hack
	it"

	^ diyAtEnd
		ifTrue: [self position >= self size]
		ifFalse: [subject atEnd].!

position
	"one of the root methods for positionable Streams"

	^ subject position.
!

position: anInteger
	"one of the root methods for positionable Streams"

	^ subject position: anInteger.!

size
	"one of the root methods for positionable Streams"

	^ subject size.!

subject: anObject
	"private -- set the object to which we forward the basic stream messages"

	super subject: anObject.

	diyAtEnd := (anObject respondsTo: #atEnd) not.
! !
!PositionableReadStreamAdaptor categoriesFor: #atEnd!public!streaming!testing! !
!PositionableReadStreamAdaptor categoriesFor: #position!positioning!public! !
!PositionableReadStreamAdaptor categoriesFor: #position:!positioning!public! !
!PositionableReadStreamAdaptor categoriesFor: #size!accessing!public! !
!PositionableReadStreamAdaptor categoriesFor: #subject:!initializing!private! !

!PositionableReadStreamAdaptor class methodsFor!

for: anObject
	"answer a new instance which wraps anObject and forwards #next:, #size, #position,
	and #position: to it.  If it also understands any of #atEnd:, #close, #next:into:startingAt:,
	or #contents then they are also forwarded"

	^ (self basicNew)
		subject: anObject;
		yourself.! !
!PositionableReadStreamAdaptor class categoriesFor: #for:!instance creation!public! !

WriteStreamAdaptor guid: (GUID fromString: '{3406EB6B-3EFF-4EE8-8DB2-2899A02B2CC5}')!
WriteStreamAdaptor comment: 'Copyright © Chris Uppal, 2002.
chris.uppal@metagnostic.org

Adaptor to make any subject which understands <#atEnd #nextPut:> function as a readable stream; if the subject also understands #close, #flush, #next:put:, #next:putAll:startingAt:, or #contents then it also forwards those.

It doesn''t forward <#size #position #position:> and so won''t act as a positionable stream (we have #shouldNotImplement for these). See PositionableWriteStreamAdaptor.'!
!WriteStreamAdaptor categoriesForClass!Unclassified! !
!WriteStreamAdaptor methodsFor!

atEnd
	"one of the root methods for <WriteStream>"

	^ subject atEnd.!

close
	"one of the root methods for <WriteStream>"

	(subject respondsTo: #close) ifTrue: [subject close].
	subject := nil.!

contents
	"if our subject understands #contents then answer the result of forwarding to it,
	otherwise throw an error"

	^ (subject respondsTo: #contents)
		ifTrue: [subject contents]
		ifFalse: [super contents].!

flush
	"one of the root methods for <WriteStream>"

	#CUtodo.  "should we precompute the test?"
	(subject respondsTo: #flush) ifTrue: [subject flush].
!

next: anInteger put: anObject
	"one of the root methods of <WriteStream>; optional because implementable in terms
	of #nextPut:"

	^ diyNextPut
		ifTrue: [super next: anInteger put: anObject]
		ifFalse: [subject next: anInteger put: anObject].
!

next: anInteger putAll: aSequenceableCollection startingAt: anIndex
	"one of the root methods of <WriteStream>; optional because implementable in terms
	of #nextPut:"

	^ diyNextPutAll
		ifTrue: [super next: anInteger putAll: aSequenceableCollection startingAt: anIndex]
		ifFalse: [subject next: anInteger putAll: aSequenceableCollection startingAt: anIndex].

!

nextPut: anObject
	"one of the root methods of <WriteStream>"

	^ subject nextPut: anObject.
!

position
	"one of the root methods for positionable Streams, which we aren't, hence it's
	forbidden"

	self shouldNotImplement.
!

position: anInteger
	"one of the root methods for positionable Streams, which we aren't, hence it's
	forbidden"

	self shouldNotImplement.!

size
	"one of the root methods for positionable Streams, which we aren't, hence it's
	forbidden"

	self shouldNotImplement.!

subject: anObject
	"private -- set the object to which we forward the basic stream messages"

	subject := anObject.

	diyNextPut := (anObject respondsTo: #next:put:) not.
	diyNextPutAll := (anObject respondsTo: #next:putAll:startingAt:) not.
	  
! !
!WriteStreamAdaptor categoriesFor: #atEnd!public!streaming!testing! !
!WriteStreamAdaptor categoriesFor: #close!operations!public! !
!WriteStreamAdaptor categoriesFor: #contents!accessing!public! !
!WriteStreamAdaptor categoriesFor: #flush!operations!public! !
!WriteStreamAdaptor categoriesFor: #next:put:!public!writing! !
!WriteStreamAdaptor categoriesFor: #next:putAll:startingAt:!public!writing! !
!WriteStreamAdaptor categoriesFor: #nextPut:!public!writing! !
!WriteStreamAdaptor categoriesFor: #position!positioning!public! !
!WriteStreamAdaptor categoriesFor: #position:!positioning!public! !
!WriteStreamAdaptor categoriesFor: #size!accessing!public! !
!WriteStreamAdaptor categoriesFor: #subject:!initializing!private! !

!WriteStreamAdaptor class methodsFor!

for: anObject
	"answer a new instance which wraps anObject and forwards #atEnd: and #nextPut: to it.
	If it also understands any of #close, #flush, #next:put:, #next:putAll:startingAt:, or #contents
	then they are also forwarded"

	^ (self basicNew)
		subject: anObject;
		yourself.! !
!WriteStreamAdaptor class categoriesFor: #for:!instance creation!public! !

StreamAdaptor guid: (GUID fromString: '{A9AEAD26-9A04-463E-B852-DD1D03D093B7}')!
StreamAdaptor comment: 'Copyright © Chris Uppal, 2002.
chris.uppal@metagnostic.org

This class might more accurately be called ''PositionableReadWriteStreamAdaptor'' -- you can probably guess why it isn''t.

Adaptor to make any subject which understands <#size #next: #nextPut: #position #position:> function as positionable stream; if the subject also understands any of #atEnd:, #close, #flush, #next:put:, #next:putAll:startingAt:, #next:into:startingAt:, or #contents then it also forwards those.'!
!StreamAdaptor categoriesForClass!Unclassified! !
!StreamAdaptor methodsFor!

atEnd
	"overridden from the superclass definition so that we can provide a fallback
	implementation (which, however, may be slow) in case the subject can't hack
	it"

	^ diyAtEnd
		ifTrue: [self position >= self size]
		ifFalse: [subject atEnd].!

close
	"one of the root methods for <{Read/Write}Stream>"

	(subject respondsTo: #close) ifTrue: [subject close].
	subject := nil.!

contents
	"if our subject understands #contents then answer the result of forwarding to it,
	otherwise throw an error"

	^ (subject respondsTo: #contents)
		ifTrue: [subject contents]
		ifFalse: [super contents].!

do: a1Block
	"if our subject understands #do: then answer the result of forwarding to it,
	otherwise execure the loop ourselves"

	^ (subject respondsTo: #do)
		ifTrue: [subject do: a1Block]
		ifFalse: [super do: a1Block].
!

flush
	"one of the root methods for <WriteStream>"

	#CUtodo.  "should we precompute the test?"
	(subject respondsTo: #flush) ifTrue: [subject flush].
!

next
	"one of the root methods for <ReadStream>"

	^ subject next.!

next: anInteger into: aSequenceableCollection startingAt: anIndex
	"one of the root methods for <ReadStream>; optional because implementable in terms
	of #next"

	^ diyNextInto
		ifTrue: [super next: anInteger into: aSequenceableCollection startingAt: anIndex]
		ifFalse: [subject next: anInteger into: aSequenceableCollection startingAt: anIndex].
!

next: anInteger put: anObject
	"one of the root methods of <WriteStream>; optional because implementable in terms
	of #nextPut:"

	^ diyNextPut
		ifTrue: [super next: anInteger put: anObject]
		ifFalse: [subject next: anInteger put: anObject].
!

next: anInteger putAll: aSequenceableCollection startingAt: anIndex
	"one of the root methods of <WriteStream>; optional because implementable in terms
	of #nextPut:"

	^ diyNextPutAll
		ifTrue: [super next: anInteger putAll: aSequenceableCollection startingAt: anIndex]
		ifFalse: [subject next: anInteger putAll: aSequenceableCollection startingAt: anIndex].

!

nextPut: anObject
	"one of the root methods of <WriteStream>"

	^ subject nextPut: anObject.
!

position
	"one of the root methods for positionable Streams"

	^ subject position.
!

position: anInteger
	"one of the root methods for positionable Streams"

	^ subject position: anInteger.!

size
	"one of the root methods for positionable Streams"

	^ subject size.!

subject: anObject
	"private -- set the object to which we forward the basic stream messages"

	subject := anObject.

	diyNextInto := (anObject respondsTo: #next:into:startingAt:) not.
	diyNextPut := (anObject respondsTo: #next:put:) not.
	diyNextPutAll := (anObject respondsTo: #next:putAll:startingAt:) not.
	diyAtEnd := (anObject respondsTo: #atEnd) not.

! !
!StreamAdaptor categoriesFor: #atEnd!public!streaming!testing! !
!StreamAdaptor categoriesFor: #close!operations!public! !
!StreamAdaptor categoriesFor: #contents!accessing!public! !
!StreamAdaptor categoriesFor: #do:!enumerating!public!streaming! !
!StreamAdaptor categoriesFor: #flush!accessing!public! !
!StreamAdaptor categoriesFor: #next!accessing!enumerating!public!streaming! !
!StreamAdaptor categoriesFor: #next:into:startingAt:!accessing!public! !
!StreamAdaptor categoriesFor: #next:put:!accessing!public! !
!StreamAdaptor categoriesFor: #next:putAll:startingAt:!accessing!public! !
!StreamAdaptor categoriesFor: #nextPut:!accessing!public! !
!StreamAdaptor categoriesFor: #position!positioning!public! !
!StreamAdaptor categoriesFor: #position:!positioning!public! !
!StreamAdaptor categoriesFor: #size!accessing!public! !
!StreamAdaptor categoriesFor: #subject:!initializing!private! !

!StreamAdaptor class methodsFor!

for: anObject
	"answer a new instance which wraps anObject and forwards #next, #nextPut:, #size, #position,
	and #position: to it.  If it also understands any of #atEnd:, #close, #flush, #next:put:,
	#next:putAll:startingAt:, #next:into:startingAt:, or #contents then they are also forwarded"

	^ (self basicNew)
		subject: anObject;
		yourself.! !
!StreamAdaptor class categoriesFor: #for:!instance creation!public! !

PositionableWriteStreamAdaptor guid: (GUID fromString: '{E5D103F4-B36C-4B9C-8676-AE8EEE0601A1}')!
PositionableWriteStreamAdaptor comment: 'Copyright © Chris Uppal, 2002.
chris.uppal@metagnostic.org

Adaptor which is the same as WriteStreamAdaptor except that it also forwards <#size #position #position:>, and provides a default implementation of #atEnd.'!
!PositionableWriteStreamAdaptor categoriesForClass!Unclassified! !
!PositionableWriteStreamAdaptor methodsFor!

atEnd
	"overridden from the superclass definition so that we can provide a fallback
	implementation (which, however, may be slow) in case the subject can't hack
	it"

	^ diyAtEnd
		ifTrue: [self position >= self size]
		ifFalse: [subject atEnd].!

position
	"one of the root methods for positionable Streams"

	^ subject position.
!

position: anInteger
	"one of the root methods for positionable Streams"

	^ subject position: anInteger.!

size
	"one of the root methods for positionable Streams"

	^ subject size.!

subject: anObject
	"private -- set the object to which we forward the basic stream messages"

	super subject: anObject.

	diyAtEnd := (anObject respondsTo: #atEnd) not.
! !
!PositionableWriteStreamAdaptor categoriesFor: #atEnd!public!streaming!testing! !
!PositionableWriteStreamAdaptor categoriesFor: #position!positioning!public! !
!PositionableWriteStreamAdaptor categoriesFor: #position:!positioning!public! !
!PositionableWriteStreamAdaptor categoriesFor: #size!accessing!public! !
!PositionableWriteStreamAdaptor categoriesFor: #subject:!initializing!private! !

!PositionableWriteStreamAdaptor class methodsFor!

for: anObject
	"answer a new instance which wraps anObject and forwards #nextPut:, #size, #position,
	and #position: to it.  If it also understands any of #atEnd:, #close, #flush, #next:put:,
	#next:putAll:startingAt:, or #contents then they are also forwarded"

	^ (self basicNew)
		subject: anObject;
		yourself.! !
!PositionableWriteStreamAdaptor class categoriesFor: #for:!instance creation!public! !

"Binary Globals"!

"Resources"!

