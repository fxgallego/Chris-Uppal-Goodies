| package |
package := Package name: 'CU Abstract Collections'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2002-2005.
chris.uppal@metagnostic.org

The standard Smalltalk Collections hierarchy is in dire need to refactoring to introduce some proper abstract superclasses for the concrete containers.  The package contains a number of classes that fake that by inheriting *from* the concrete container, and then overriding enough methods (back to subclassResponsibility) to act as a reasonable base class for further concrete classes that implement the same protocol.

Please note that a large fraction of this package is code that is copied from the Dolphin base image (see Abstract*Stream specially); such code is, of course, *not* covered by the above copyright notice, nor by the following terms.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris


History:

4.00
-	Added equivalents of the extra methods in ''CU Stream Extensions''.

3.00
-	Updated AbstractStream>>nextLine to reflect implementation in SequencedStream.

2.00
-	Updated Abstract*Stream>>nextWord to reflect improved implementation in D5.1.

1.00
-	First release.
'.

package basicPackageVersion: '4.02'.


package classNames
	add: #AbstractArray;
	add: #AbstractCollection;
	add: #AbstractDictionary;
	add: #AbstractOrderedCollection;
	add: #AbstractPositionableStream;
	add: #AbstractReadStream;
	add: #AbstractReadWriteStream;
	add: #AbstractSet;
	add: #AbstractWriteStream;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Base\Dolphin';
	yourself).

package!

"Class Definitions"!

Collection subclass: #AbstractCollection
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
SequenceableCollection subclass: #AbstractArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
SequencedGrowableCollection subclass: #AbstractOrderedCollection
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Set variableSubclass: #AbstractSet
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Dictionary variableSubclass: #AbstractDictionary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Stream subclass: #AbstractPositionableStream
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
AbstractPositionableStream subclass: #AbstractReadStream
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
AbstractPositionableStream subclass: #AbstractWriteStream
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
AbstractWriteStream subclass: #AbstractReadWriteStream
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

AbstractCollection guid: (GUID fromString: '{83F17EE0-7703-4FAE-8EFA-4EA6D55E00D9}')!
AbstractCollection comment: 'Copyright © Chris Uppal, 2002.
chris.uppal@metagnostic.org

There is little need for this class since Collection is already abstract.  However it serves:
a)	to maintain consistancy with the other collections abstractor classes
b)	to document what is needed to make the abstraction concrete.'!
!AbstractCollection categoriesForClass!Unclassified! !
!AbstractCollection methodsFor!

add: anObject
	"one of the root methods for <Collection>"

	self subclassResponsibility.!

copyEmpty: anInteger
	"private -overriden to use species (which it probably should have done in the first place)"

	^ self species new: anInteger.
!

do: a1Block
	"one of the root methods for <Collection>"

	self subclassResponsibility.!

includes: anObject
	"one of the root methods of Collection; optional because it can be implemented
	in terms of #do:"

	"default implementation is provided by superclass"
	^ super includes: anObject.!

remove: anObject ifAbsent: a0Block
	"one of the root methods for <Collection>"

	self subclassResponsibility.!

size
	"one of the root methods for <Collection>"

	self subclassResponsibility.!

species
	"answer the kind of collection to be used when copying the receiver"

	"it is arguable that the fundamental concrete container is actually Bag, however
	it seems a little perverse, so I've choosen to use"
	^ OrderedCollection.! !
!AbstractCollection categoriesFor: #add:!adding!must implement!public! !
!AbstractCollection categoriesFor: #copyEmpty:!copying!private! !
!AbstractCollection categoriesFor: #do:!enumerating!must implement!public! !
!AbstractCollection categoriesFor: #includes:!public!searching!should override! !
!AbstractCollection categoriesFor: #remove:ifAbsent:!must implement!public!removing! !
!AbstractCollection categoriesFor: #size!accessing!must implement!public! !
!AbstractCollection categoriesFor: #species!constants!public! !

!AbstractCollection class methodsFor!

new
	"private -- this class is abstract and should never be instanciated"

	self shouldNotImplement.!

new: anInteger
	"private -- this class is abstract and should never be instanciated"

	self shouldNotImplement.! !
!AbstractCollection class categoriesFor: #new!instance creation!private! !
!AbstractCollection class categoriesFor: #new:!instance creation!private! !

AbstractArray guid: (GUID fromString: '{EB89D272-FDD6-47EE-949B-DA867C7FA6EF}')!
AbstractArray comment: 'Copyright © Chris Uppal, 2002.
chris.uppal@metagnostic.org

There is little need for this class since SequenceableCollection is already abstract.  However it serves:
a)	to maintain consistancy with the other collections abstractor classes
b)	to document what is needed to make the abstraction concrete.'!
!AbstractArray categoriesForClass!Unclassified! !
!AbstractArray methodsFor!

add: anObject
	"this should be inherited from the superclass, but it's missing"

	self shouldNotImplement.!

at: anInteger
	"one of the root methods for <SequenceableCollection>"

	self subclassResponsibility.!

at: anInteger put: anObject
	"one of the root methods for <SequenceableCollection>"

	self subclassResponsibility.!

from: aStartIndex to: aStopIndex keysAndValuesDo: a2Block
	"one of the root methods for <SequenceableCollection>, optional because it can be
	implemented in terms of #at: and #size"

	"default implementation is provided by superclass"
	^ super from: aStartIndex to: aStopIndex keysAndValuesDo: a2Block.!

replaceFrom: aStartIndex to: aStopIndex with: replacementElements startingAt: aReplacementIndex
	"one of the root methods for <SequenceableCollection>, optional because it can be
	implemented in terms of #at:, #at:put:, and #size"

	"default implementation is provided by superclass"
	^ super replaceFrom: aStartIndex to: aStopIndex with: replacementElements startingAt: aReplacementIndex.!

resize: anInteger
	"private -- overriden to ignore"
!

size
	"one of the root methods for <Collection>"

	self subclassResponsibility.!

species
	"answer the kind of collection to be used when copying the receiver"

	^ Array.! !
!AbstractArray categoriesFor: #add:!adding!public! !
!AbstractArray categoriesFor: #at:!accessing!must implement!public! !
!AbstractArray categoriesFor: #at:put:!accessing!must implement!public! !
!AbstractArray categoriesFor: #from:to:keysAndValuesDo:!enumerating!public!should override! !
!AbstractArray categoriesFor: #replaceFrom:to:with:startingAt:!public!replacing!should override! !
!AbstractArray categoriesFor: #resize:!mutating!private! !
!AbstractArray categoriesFor: #size!accessing!must implement!public! !
!AbstractArray categoriesFor: #species!constants!public! !

!AbstractArray class methodsFor!

new
	"private -- this class is abstract and should never be instanciated"

	self shouldNotImplement.!

new: anInteger
	"private -- this class is abstract and should never be instanciated"

	self shouldNotImplement.! !
!AbstractArray class categoriesFor: #new!instance creation!private! !
!AbstractArray class categoriesFor: #new:!instance creation!private! !

AbstractOrderedCollection guid: (GUID fromString: '{92548135-E886-4B3E-BFA0-5FF30A886BF3}')!
AbstractOrderedCollection comment: 'Copyright © Chris Uppal, 2002.
chris.uppal@metagnostic.org

There is little need for this class since SequenceableGrowableCollection is already abstract.  However it serves:
a)	to maintain consistancy with the other collections abstractor classes
b)	to document what is needed to make the abstraction concrete.'!
!AbstractOrderedCollection categoriesForClass!Unclassified! !
!AbstractOrderedCollection methodsFor!

add: anObject afterIndex: anIndex
	"one of the root methods for <SequencedGrowableCollection>"

	self subclassResponsibility.
!

at: anIndex
	"one of the root methods for <SequenceableCollection>"

	self subclassResponsibility.
!

at: anIndex put: anObject
	"one of the root methods for <SequenceableCollection>"

	self subclassResponsibility.
!

from: aStartIndex to: aStopIndex keysAndValuesDo: a2Block
	"one of the root methods for <SequenceableCollection>, optional because it can be
	implemented in terms of #at: and #size"

	"default implementation is provided by superclass"
	^ super from: aStartIndex to: aStopIndex keysAndValuesDo: a2Block.!

removeAll
	"one of the root methods for <SequencedGrowableCollection>, optional because it can be
	implemented in terms of #removeAtIndex: and #size"

	| size | 

	"I'm not sure why the superclass doesn't provide a default implementation of this.  Possibly
	because ti's difficult to know whether remove-first or remove-last is more efficient, however
	most actual containers can remove-last at least as quickly as remove-first; singly-linked lists
	are the main exception and they are rarely used (and can override this anyway)."
	[(size := self size) > 0] whileTrue: [self removeAtIndex: size].!

removeAtIndex: anIndex
	"one of the root methods for <SequencedGrowableCollection>"

	self subclassResponsibility.
!

replaceFrom: aStartIndex to: aStopIndex with: replacementElements startingAt: aReplacementIndex
	"one of the root methods for <SequenceableCollection>, optional because it can be
	implemented in terms of #at:, #at:put:, and #size"

	"default implementation is provided by superclass"
	^ super replaceFrom: aStartIndex to: aStopIndex with: replacementElements startingAt: aReplacementIndex.!

resize: anInteger
	"private -- overridden to ignore"
!

size
	"one of the root methods for <Collection>"

	self subclassResponsibility.
!

species
	"answer the kind of collection to be used when copying the receiver"

	^ OrderedCollection.! !
!AbstractOrderedCollection categoriesFor: #add:afterIndex:!adding!must implement!public! !
!AbstractOrderedCollection categoriesFor: #at:!accessing!must implement!public! !
!AbstractOrderedCollection categoriesFor: #at:put:!accessing!must implement!public! !
!AbstractOrderedCollection categoriesFor: #from:to:keysAndValuesDo:!enumerating!public!should override! !
!AbstractOrderedCollection categoriesFor: #removeAll!public!removing!should override! !
!AbstractOrderedCollection categoriesFor: #removeAtIndex:!must implement!public!removing! !
!AbstractOrderedCollection categoriesFor: #replaceFrom:to:with:startingAt:!public!replacing!should override! !
!AbstractOrderedCollection categoriesFor: #resize:!mutating!private! !
!AbstractOrderedCollection categoriesFor: #size!accessing!must implement!public! !
!AbstractOrderedCollection categoriesFor: #species!constants!public! !

!AbstractOrderedCollection class methodsFor!

new
	"private -- this class is abstract and should never be instanciated"

	self shouldNotImplement.!

new: anInteger
	"private -- this class is abstract and should never be instanciated"

	self shouldNotImplement.! !
!AbstractOrderedCollection class categoriesFor: #new!instance creation!private! !
!AbstractOrderedCollection class categoriesFor: #new:!instance creation!private! !

AbstractSet guid: (GUID fromString: '{88068EED-3C82-4C8F-9507-BA5F69F545DD}')!
AbstractSet comment: 'Copyright © Chris Uppal, 2002.
chris.uppal@metagnostic.org

The purpose of this class is to try to abstract away the details of the concrete class Set, and end up with an abstract class with the same interface as Set, and a small number of #subclassResponsibility methods which other concrete implementations of the Set interface should provide.'!
!AbstractSet categoriesForClass!Unclassified! !
!AbstractSet methodsFor!

add: anObject
	"one of the root methods for <Collection>"

	self subclassResponsibility.!

asSet
	"overriden so that this can be used to turn us back into a real Set"

	^ (Set new: self size) 
		addAll: self; 
		yourself.
!

copyEmpty: anInteger
	"private - overriden to use #species (which it probably should have done in the first place)"

	^ self species new: anInteger.
!

copyWithoutDuplicates
	"answers a copy of the receiver that contains no duplicate objects"

	^ self asSet.!

do: a1Block
	"one of the root methods for <Collection>"

	self subclassResponsibility.!

findElementOrNil: anObject

	"if this gets called then there is some leakage in the way I've tried to force Set back into an abstraction"
	self shouldNotImplement.!

grow

	"overridden to ignore"
!

identityIncludes: anObject
	"overriden back to an abstract implementation"

	^ self anySatisfy: [:each | each == anObject].!

includes: anObject
	"one of the root methods for <Set>"

	self subclassResponsibility.!

rehash
	"it could be argued that this has no place in the public contract of an abstraction,
	but all concrete implementations need to publish #rehash"

	"default is to ignore"
!

remove: anObject ifAbsent: a0Block
	"one of the root methods for <Collection>"

	self subclassResponsibility.!

resize: anInteger

	"overridden to ignore"!

shrink
	"it could be argued that this has no place in the public contract of an abstraction,
	but all concrete implementations need to publish #shrink"

	"default is to ignore"!

size
	"one of the root methods for <Collection>"

	self subclassResponsibility.
!

species
	"answer the kind of collection to be used when copying the receiver"

	^ Set.!

stbSaveOn: anSTBOutFiler
	"overriden back to the default Object behavior"

	anSTBOutFiler saveObject: self.! !
!AbstractSet categoriesFor: #add:!adding!must implement!public! !
!AbstractSet categoriesFor: #asSet!converting!public! !
!AbstractSet categoriesFor: #copyEmpty:!copying!private! !
!AbstractSet categoriesFor: #copyWithoutDuplicates!copying!public! !
!AbstractSet categoriesFor: #do:!enumerating!must implement!public! !
!AbstractSet categoriesFor: #findElementOrNil:!private!searching! !
!AbstractSet categoriesFor: #grow!adding!private! !
!AbstractSet categoriesFor: #identityIncludes:!public!searching! !
!AbstractSet categoriesFor: #includes:!must implement!public!searching! !
!AbstractSet categoriesFor: #rehash!operations!public!should override! !
!AbstractSet categoriesFor: #remove:ifAbsent:!must implement!public!removing! !
!AbstractSet categoriesFor: #resize:!mutating!private! !
!AbstractSet categoriesFor: #shrink!mutating!public!should override! !
!AbstractSet categoriesFor: #size!accessing!must implement!public! !
!AbstractSet categoriesFor: #species!constants!public! !
!AbstractSet categoriesFor: #stbSaveOn:!binary filing!public! !

!AbstractSet class methodsFor!

new
	"private -- this class is abstract and should never be instanciated"

	self shouldNotImplement.!

new: anInteger
	"private -- this class is abstract and should never be instanciated"

	self shouldNotImplement.! !
!AbstractSet class categoriesFor: #new!instance creation!private! !
!AbstractSet class categoriesFor: #new:!instance creation!private! !

AbstractDictionary guid: (GUID fromString: '{99EF87E7-A300-450B-8F6F-F753A39B09ED}')!
AbstractDictionary comment: 'Copyright © Chris Uppal, 2002.
chris.uppal@metagnostic.org

The purpose of this class is to try to abstract away the details of the concrete class Dictionary, and end up with an abstract class with the same interface as Dictionary, and a small number of #subclassResponsibility methods which other concrete implementations of the Set interface should provide.'!
!AbstractDictionary categoriesForClass!Unclassified! !
!AbstractDictionary methodsFor!

add: anAssociation
	"one of the root methods for <Collection>.  Overriden to 'know' about bloody Associations"

	^ self at: anAssociation key put: anAssociation value.
!

associationAt: aKey ifAbsent: a0Block
	"this association crap is the absolute limit"

	| val |

	val := self at: aKey ifAbsent: [^ a0Block value].

	^ aKey -> val.!

associationsDo: a1Block

	^ self keysAndValuesDo: [:key :value | a1Block value: key -> value].!

at: aKey ifAbsent: a0Block
	"one of the root methods for <Dictionary>"

	self subclassResponsibility.!

at: aKey put: anObject
	"one of the root methods for <Dictionary>"

	self subclassResponsibility.
!

copyEmpty: anInteger
	"private - overriden to use #species (which it probably should have done in the first place)"

	^ self species new: anInteger.
!

copyWithoutDuplicates
	"answers a copy of the receiver that contains no duplicate objects"

	"I've no idea what this even means for Dictionaries"

	^ self copy.!

do: a1Block

	^ self keysAndValuesDo: [:key :value | a1Block value: value].!

find: target ifAbsent: a0Block

	"I've no idea what this even means for Dictionaries"

	^ a0Block value.
!

findElementOrNil: anObject

	"if this gets called then there is some leakage in the way I've tried to force Dictionary back into an abstraction"
	self shouldNotImplement.!

findKeyOrNil: key
 
	"if this gets called then there is some leakage in the way I've tried to force Dictionary back into an abstraction"
	self shouldNotImplement.
!

grow

	"overridden to ignore"
!

includesAssociation: anAssociation
	"this association crap is the absolute limit"

	^ (self at: anAssociation key ifAbsent: [^ false]) = anAssociation value.
!

includesKey: aKey
	"one of the root methods for <Dictionary>, optional because implementable in terms
	of at:ifAbsent:"

	"default implementation"
	self at: aKey ifAbsent: [^ false].
	^ true.!

keysAndValuesDo: a2Block
	"one of the root methods for <Dictionary>"

	self subclassResponsibility.
!

rehash
	"it could be agrued that this has no place in the public contract of an abstraction,
	but all concrete implementations need to publish #rehash"

	"default is to ignore"
!

removeKey: aKey ifAbsent: a0Block
	"one of the root methods for <Dictionary>"

	self subclassResponsibility.
!

resize: anInteger

	"overridden to ignore"!

shrink
	"it could be agrued that this has no place in the public contract of an abstraction,
	but all concrete implementations need to publish #shrink"

	"default is to ignore"!

size
	"one of the root methods for <Collection>"

	self subclassResponsibility.!

species
	"answer the kind of collection to be used when copying the receiver"

	^ LookupTable.!

stbSaveOn: anSTBOutFiler
	"overriden back to the default Object behavior"

	anSTBOutFiler saveObject: self.! !
!AbstractDictionary categoriesFor: #add:!adding!public! !
!AbstractDictionary categoriesFor: #associationAt:ifAbsent:!accessing!public! !
!AbstractDictionary categoriesFor: #associationsDo:!enumerating!public! !
!AbstractDictionary categoriesFor: #at:ifAbsent:!accessing!must implement!public! !
!AbstractDictionary categoriesFor: #at:put:!accessing!must implement!public! !
!AbstractDictionary categoriesFor: #copyEmpty:!copying!private! !
!AbstractDictionary categoriesFor: #copyWithoutDuplicates!copying!public! !
!AbstractDictionary categoriesFor: #do:!enumerating!public! !
!AbstractDictionary categoriesFor: #find:ifAbsent:!public!searching! !
!AbstractDictionary categoriesFor: #findElementOrNil:!private!searching! !
!AbstractDictionary categoriesFor: #findKeyOrNil:!private!searching! !
!AbstractDictionary categoriesFor: #grow!adding!private! !
!AbstractDictionary categoriesFor: #includesAssociation:!public!searching! !
!AbstractDictionary categoriesFor: #includesKey:!public!searching!should override! !
!AbstractDictionary categoriesFor: #keysAndValuesDo:!enumerating!must implement!public! !
!AbstractDictionary categoriesFor: #rehash!operations!public!should override! !
!AbstractDictionary categoriesFor: #removeKey:ifAbsent:!must implement!public!removing! !
!AbstractDictionary categoriesFor: #resize:!mutating!private! !
!AbstractDictionary categoriesFor: #shrink!mutating!public!should override! !
!AbstractDictionary categoriesFor: #size!accessing!must implement!public! !
!AbstractDictionary categoriesFor: #species!constants!public! !
!AbstractDictionary categoriesFor: #stbSaveOn:!binary filing!public! !

!AbstractDictionary class methodsFor!

new
	"private -- this class is abstract and should never be instanciated"

	self shouldNotImplement.!

new: anInteger
	"private -- this class is abstract and should never be instanciated"

	self shouldNotImplement.! !
!AbstractDictionary class categoriesFor: #new!instance creation!private! !
!AbstractDictionary class categoriesFor: #new:!instance creation!private! !

AbstractPositionableStream guid: (GUID fromString: '{DE068A2A-D322-45FB-857C-9E8102943278}')!
AbstractPositionableStream comment: 'Copyright © Chris Uppal, 2002.
chris.uppal@metagnostic.org'!
!AbstractPositionableStream categoriesForClass!Unclassified! !
!AbstractPositionableStream methodsFor!

atEnd
	"one of the root methods for positionable streams"

	^ self position >= self size.
!

isEmpty

	^ self size = 0.!

pop
	"copied from PositionableStream"

	self skip: -1.!

position
	"one of the root methods for positionable Streams"

	self subclassResponsibility.
!

position: anInteger
	"one of the root methods for positionable Streams"

	self subclassResponsibility.
!

reset
	"copied from PositionableStream"

	self position: 0.!

setToEnd

	self position: self size.!

size
	"one of the root methods for positionable Streams"

	"arguably we could provide a default implementation using #position[:] and #next,
	but it'd be too ineficient"
	self subclassResponsibility.!

skip: anInteger
	"copied from PositionableStream (well, actually from FileStream)"

	self position: (((self position + anInteger) max: 0) min: self size - 1).! !
!AbstractPositionableStream categoriesFor: #atEnd!public!should override!streaming!testing! !
!AbstractPositionableStream categoriesFor: #isEmpty!public!testing! !
!AbstractPositionableStream categoriesFor: #pop!positioning!public! !
!AbstractPositionableStream categoriesFor: #position!must implement!positioning!public! !
!AbstractPositionableStream categoriesFor: #position:!must implement!positioning!public! !
!AbstractPositionableStream categoriesFor: #reset!positioning!public! !
!AbstractPositionableStream categoriesFor: #setToEnd!positioning!public! !
!AbstractPositionableStream categoriesFor: #size!accessing!must implement!public! !
!AbstractPositionableStream categoriesFor: #skip:!positioning!public! !

!AbstractPositionableStream class methodsFor!

new
	"private -- this class is abstract and should never be instanciated"

	self shouldNotImplement.! !
!AbstractPositionableStream class categoriesFor: #new!instance creation!private! !

AbstractReadStream guid: (GUID fromString: '{025A4890-A6C8-4118-877F-AD649CCEF190}')!
AbstractReadStream comment: 'Copyright © Chris Uppal, 2002.
chris.uppal@metagnostic.org'!
!AbstractReadStream categoriesForClass!Unclassified! !
!AbstractReadStream methodsFor!

atEnd
	"one of the root methods for <ReadStream>"

	"it's not obvious whether we should inherit the implementation from
	AbstractPositionableStream or make it #subclassResponsibility"
	^ super atEnd.
!

close
	"one of the root methods for <ReadStream>"

	"default implementation is to ignore"
!

contents
	"it's not clear that this is really part of the external contract of a readable stream
	abstraction.  It seems to be appropriate only to external iterators (which 'know' that
	there's an underlying container of some sort), however ANSI makes it a part of
	<sequencedStream>, and hence <ReadStream>, so we'll include it here"

	| pos answer |

	pos := self position.
	self reset.
	answer := self upToEnd.
	self position: pos.

	^ answer.!

do: a1Block
	"one of the root methods for <ReadStream>; optional because implementable in terms
	of #atEnd and #next"

	"default implementation is provided by superclass"
	^ super do: a1Block.!

isReadable
	"one of the root methods for <ReadStream>"

	^ true.!

isWriteable
	"one of the root methods for <ReadStream>"

	^ false.!

next
	"one of the root methods for <ReadStream>"

	self subclassResponsibility.
!

next: anInteger into: aSequenceableCollection startingAt: anIndex
	"one of the root methods for <ReadStream>; optional because implementable in terms
	of #next"

	"default implementation is provided by superclass"
	^ super next: anInteger into: aSequenceableCollection startingAt: anIndex.!

nextBeDOUBLE
	"copied from PositionableStream"

	| bytes |

	bytes := (ByteArray new: 8)
			at: 8 put: (self next);
			at: 7 put: (self next);
			at: 6 put: (self next);
			at: 5 put: (self next);
			at: 4 put: (self next);
			at: 3 put: (self next);
			at: 2 put: (self next);
			at: 1 put: (self next);
			yourself.
	^ bytes doubleAtOffset: 0!

nextBeDWORD
	"copied from PositionableStream"

#CUadded.
	^ (((self next bitShift: 24)
		bitOr: (self next bitShift: 16))
			bitOr: (self next bitShift: 8))
				bitOr: self next.!

nextBeFLOAT
	"copied from PositionableStream"

	| bytes |

#CUadded.
	bytes := (ByteArray new: 4)
			at: 4 put: (self next);
			at: 3 put: (self next);
			at: 2 put: (self next);
			at: 1 put: (self next);
			yourself.
	^ bytes floatAtOffset: 0!

nextBeQWORD
	"copied from PositionableStream"

	| bytes |

#CUadded.
	bytes := (ByteArray new: 8)
			at: 8 put: (self next);
			at: 7 put: (self next);
			at: 6 put: (self next);
			at: 5 put: (self next);
			at: 4 put: (self next);
			at: 3 put: (self next);
			at: 2 put: (self next);
			at: 1 put: (self next);
			yourself.
	^ bytes qwordAtOffset: 0!

nextBeSDWORD
	"copied from PositionableStream"

	| bytes |

#CUadded.
	bytes := (ByteArray new: 4)
			at: 4 put: (self next);
			at: 3 put: (self next);
			at: 2 put: (self next);
			at: 1 put: (self next);
			yourself.
	^ bytes sdwordAtOffset: 0!

nextBeSQWORD
	"copied from PositionableStream"

	| bytes |

#CUadded.
	bytes := (ByteArray new: 8)
			at: 8 put: (self next);
			at: 7 put: (self next);
			at: 6 put: (self next);
			at: 5 put: (self next);
			at: 4 put: (self next);
			at: 3 put: (self next);
			at: 2 put: (self next);
			at: 1 put: (self next);
			yourself.
	^ bytes sqwordAtOffset: 0!

nextBeSWORD
	"copied from PositionableStream"

	| int |
#CUadded.

	int := self nextBeWORD.
	^ int >= 16r8000
		ifTrue: [int - 16r10000]
		ifFalse: [int].!

nextBeWORD
	"copied from PositionableStream"

#CUadded.
	^ (self next bitShift: 8) bitOr: self next.
!

nextDOUBLE
	"copied from PositionableStream"

	^ (self next: 8) doubleAtOffset: 0.!

nextDWORD
	"copied from PositionableStream"

	^ (self next: 4) dwordAtOffset: 0.!

nextFLOAT
	"copied from PositionableStream"

	^ (self next: 4) floatAtOffset: 0.!

nextInto: aSequenceableCollection
	"copied from PositionableStream"

	^ self next: aSequenceableCollection size into: aSequenceableCollection startingAt: 1.!

nextLine
	"copied from SequencedStream"

	| result eol |

	eol := String lineDelimiter.
	result := self upTo: eol last.

	^ (result notEmpty and: [result last = eol first]) 
		ifTrue: [result allButLast]
		ifFalse: [result].!

nextQWORD
	"copied from PositionableStream"

	^ (self next: 8) qwordAtOffset: 0.!

nextSDWORD
	"copied from PositionableStream"

	^ (self next: 4) sdwordAtOffset: 0.!

nextSQWORD
	"copied from PositionableStream"

	^ (self next: 8) sqwordAtOffset: 0.!

nextSWORD
	"copied from PositionableStream"

	^ (self next: 2) swordAtOffset: 0.!

nextWord
	"copied from SequencedStream"

	| wordStream element |

	self skipSeparators ifFalse: [^ nil].
	wordStream := self contentsSpecies writeStream: 32.
	[self atEnd] whileFalse:
		[element := self next.
		element isSeparator ifTrue: [^ wordStream contents].
		wordStream nextPut: element].

	^ wordStream contents.!

nextWORD
	"copied from PositionableStream"

	^ (self next: 2) wordAtOffset: 0.!

peek
	"copied from PositionableStream"

	| anObject |

	self atEnd ifTrue: [^ nil].

	anObject := self next.
	self pop.

	^ anObject.!

peekFor: anObject
	"copied from PositionableStream"

	^ self peek = anObject
		ifTrue: [self next. true]
		ifFalse: [false].!

reverseContents
	"copied from PositionableStream (well, actually from FileStream)"

	^ (self contents readStream) reverseContents.
!

skipSeparators
	"copied from PositionableStream"

	^ self skipWhile: [:c | c isSeparator].!

skipWhile: a1Block
	"copied from PositionableStream"

	[self atEnd] whileFalse:
		[(a1Block value: self next) ifFalse: [self pop. ^ true]].

	^ false.
!

skipWhileMatchAll: aCollection
	"copied from PositionableStream"

	^ aCollection skipOver: self.! !
!AbstractReadStream categoriesFor: #atEnd!must implement!public!should override!streaming!testing! !
!AbstractReadStream categoriesFor: #close!operations!public!should override! !
!AbstractReadStream categoriesFor: #contents!accessing!public!should override! !
!AbstractReadStream categoriesFor: #do:!enumerating!public!should override!streaming! !
!AbstractReadStream categoriesFor: #isReadable!public!testing! !
!AbstractReadStream categoriesFor: #isWriteable!public!testing! !
!AbstractReadStream categoriesFor: #next!accessing!enumerating!must implement!public!streaming! !
!AbstractReadStream categoriesFor: #next:into:startingAt:!accessing!public!should override! !
!AbstractReadStream categoriesFor: #nextBeDOUBLE!binary filing!public! !
!AbstractReadStream categoriesFor: #nextBeDWORD!binary filing!public! !
!AbstractReadStream categoriesFor: #nextBeFLOAT!binary filing!public! !
!AbstractReadStream categoriesFor: #nextBeQWORD!binary filing!public! !
!AbstractReadStream categoriesFor: #nextBeSDWORD!binary filing!public! !
!AbstractReadStream categoriesFor: #nextBeSQWORD!binary filing!public! !
!AbstractReadStream categoriesFor: #nextBeSWORD!binary filing!public! !
!AbstractReadStream categoriesFor: #nextBeWORD!binary filing!public! !
!AbstractReadStream categoriesFor: #nextDOUBLE!binary filing!public! !
!AbstractReadStream categoriesFor: #nextDWORD!binary filing!public! !
!AbstractReadStream categoriesFor: #nextFLOAT!binary filing!public! !
!AbstractReadStream categoriesFor: #nextInto:!accessing!public! !
!AbstractReadStream categoriesFor: #nextLine!accessing!public! !
!AbstractReadStream categoriesFor: #nextQWORD!binary filing!public! !
!AbstractReadStream categoriesFor: #nextSDWORD!binary filing!public! !
!AbstractReadStream categoriesFor: #nextSQWORD!binary filing!public! !
!AbstractReadStream categoriesFor: #nextSWORD!binary filing!public! !
!AbstractReadStream categoriesFor: #nextWord!accessing!public! !
!AbstractReadStream categoriesFor: #nextWORD!binary filing!public! !
!AbstractReadStream categoriesFor: #peek!accessing!public! !
!AbstractReadStream categoriesFor: #peekFor:!accessing!public! !
!AbstractReadStream categoriesFor: #reverseContents!accessing!public! !
!AbstractReadStream categoriesFor: #skipSeparators!positioning!public! !
!AbstractReadStream categoriesFor: #skipWhile:!positioning!public! !
!AbstractReadStream categoriesFor: #skipWhileMatchAll:!positioning!public! !

AbstractWriteStream guid: (GUID fromString: '{AC4C6A46-14FB-4943-8294-698301A20C03}')!
AbstractWriteStream comment: 'Copyright © Chris Uppal, 2002.
chris.uppal@metagnostic.org'!
!AbstractWriteStream categoriesForClass!Unclassified! !
!AbstractWriteStream methodsFor!

atEnd
	"one of the root methods for <ReadStream>"

	"it's not obvious whether we should inherit the implementation from
	AbstractPositionableStream or make it #subclassResponsibility"
	^ super atEnd.
!

basicPrint: anObject
	"copied from WriteStream"

	anObject basicPrintOn: self.!

binaryStore: anObject
	"copied from WriteStream"

	anObject binaryStoreOn: self.
!

close
	"one of the root methods for <WriteStream>"

	"default implementation is to ignore"
!

comma
	"copied from WriteStream"

	#deprecated.
	self nextPut: $,.!

contents
	"it's not clear that this is really part of the external contract of a writable stream
	abstraction.  It seems to be appropriate only to external iterators (which 'know' that
	there's an underlying container of some sort), however ANSI makes it a part of
	<sequencedStream>, and hence <WriteStream>, so we'll include it here, but make
	it throw an error"

	self shouldNotImplement.!

cr
	"copied from WriteStream"

	self nextPutAll: String lineDelimiter.!

crtab
	"copied from WriteStream"

	self cr; tab.!

crtab: anInteger
	"copied from WriteStream"

	self
		cr;
		tab: anInteger.!

display: anObject
	"copied from WriteStream"

	anObject displayOn: self.!

flush

	"default is to ignore"
!

isReadable
	"one of the root methods for <WriteStream>"

	^ false.!

isWriteable
	"one of the root methods for <WriteStream>"

	^ true.!

next
	"one of the root methods for <ReadStream>, overridden to remove from <WriteStream>"

	self shouldNotImplement.!

next: anInteger put: anObject
	"one of the root methods of <WriteStream>; optional because implementable in terms
	of #nextPut:"

	anInteger timesRepeat: [self nextPut: anObject].

	^ anObject.!

next: anInteger putAll: aSequenceableCollection startingAt: anIndex
	"one of the root methods of <WriteStream>; optional because implementable in terms
	of #nextPut:"

	anIndex
		to: anIndex + anInteger - 1
		do: [:i | self nextPut: (aSequenceableCollection at: i)].

	^ aSequenceableCollection.!

nextBeDOUBLEPut: aFloat
	"copied from WriteStream"

	| bytes |

	bytes := (ByteArray new: 8)
			doubleAtOffset: 0 put: aFloat;
			yourself.
	8 to: 1 by: -1 do: [:i | self nextPut: (bytes at: i)].

	^ aFloat.!

nextBeDWORDPut: anInteger
	"copied from WriteStream"

	| bytes |

	(anInteger < 0) ifTrue: [^ self errorCantHold: anInteger].	"#dwordAtOffset:put: doesn't check for negative numbers"
	bytes := (ByteArray new: 4)
			dwordAtOffset: 0 put: anInteger;
			yourself.
	4 to: 1 by: -1 do: [:i | self nextPut: (bytes at: i)].

	^ anInteger.
!

nextBeFLOATPut: aFloat
	"copied from WriteStream"

	| bytes |

	bytes := (ByteArray new: 4)
			floatAtOffset: 0 put: aFloat;
			yourself.
	4 to: 1 by: -1 do: [:i | self nextPut: (bytes at: i)].

	^ aFloat.!

nextBeQWORDPut: anInteger
	"copied from WriteStream"

	| bytes |

	(anInteger < 0) ifTrue: [^ self errorCantHold: anInteger].	"#qwordAtOffset:put: doesn't check for negative numbers"
	bytes := (ByteArray new: 8)
			qwordAtOffset: 0 put: anInteger;
			yourself.
	8 to: 1 by: -1 do: [:i | self nextPut: (bytes at: i)].

	^ anInteger.
!

nextBeSDWORDPut: anInteger
	"copied from WriteStream"

	| bytes |

	bytes := (ByteArray new: 4)
			sdwordAtOffset: 0 put: anInteger;
			yourself.
	4 to: 1 by: -1 do: [:i | self nextPut: (bytes at: i)].

	^ anInteger.
!

nextBeSQWORDPut: anInteger
	"copied from WriteStream"

	| bytes |

	bytes := (ByteArray new: 8)
			sqwordAtOffset: 0 put: anInteger;
			yourself.
	8 to: 1 by: -1 do: [:i | self nextPut: (bytes at: i)].

	^ anInteger.
!

nextBeSWORDPut: anInteger
	"copied from WriteStream"

	(anInteger >= -16r8000 and: [anInteger < 16r8000]) ifFalse: [^ self errorCantHold: anInteger].
	self
		nextPut: ((anInteger bitShift: -8) bitAnd: 16rFF);	
		nextPut: (anInteger bitAnd: 16rFF).

	^ anInteger.
!

nextBeWORDPut: anInteger
	"copied from WriteStream"

	(anInteger >= 0 and: [anInteger <= 16rFFFF]) ifFalse: [^ self errorCantHold: anInteger].
	self
		nextPut: (anInteger bitShift: -8);
		nextPut: (anInteger bitAnd: 16rFF).

	^ anInteger.
!

nextDOUBLEPut: aFloat
	"copied from WriteStream"

	self nextPutAll: 
		((ByteArray new: 8)
			doubleAtOffset: 0 put: aFloat;
			yourself).

	^ aFloat.!

nextDWORDPut: anInteger
	"copied from WriteStream"

	(anInteger < 0) ifTrue: [^ self errorCantHold: anInteger].	"#dwordAtOffset:put: doesn't check for negative numbers"
	self nextPutAll: 
		((ByteArray new: 4)
			dwordAtOffset: 0 put: anInteger;
			yourself).

	^ anInteger.!

nextFLOATPut: aFloat
	"copied from WriteStream"

	self nextPutAll: 
		((ByteArray new: 4)
			floatAtOffset: 0 put: aFloat;
			yourself).

	^ aFloat.!

nextPut: anObject
	"one of the root methods of <WriteStream>"

	self subclassResponsibility.
!

nextPutAll: aCollection
	"copied from WriteStream"

	^ aCollection appendToStream: self.!

nextQWORDPut: anInteger
	"copied from WriteStream"

	(anInteger < 0) ifTrue: [^ self errorCantHold: anInteger].	"#qwordAtOffset:put: doesn't check for negative numbers"
	self nextPutAll: 
		((ByteArray new: 8)
			qwordAtOffset: 0 put: anInteger;
			yourself).

	^ anInteger.!

nextSDWORDPut: anInteger
	"copied from WriteStream"

	self nextPutAll: 
		((ByteArray new: 4)
			sdwordAtOffset: 0 put: anInteger;
			yourself).

	^ anInteger.!

nextSQWORDPut: anInteger
	"copied from WriteStream"

	self nextPutAll: 
		((ByteArray new: 8)
			sqwordAtOffset: 0 put: anInteger;
			yourself).

	^ anInteger.!

nextSWORDPut: anInteger
	"copied from WriteStream"

	(anInteger >= -16r8000 and: [anInteger < 16r8000]) ifFalse: [^ self errorCantHold: anInteger].
	self
		nextPut: (anInteger bitAnd: 16rFF);
		nextPut: ((anInteger bitShift: -8) bitAnd: 16rFF).

	^ anInteger.!

nextWORDPut: anInteger
	"copied from WriteStream"

	(anInteger >= 0 and: [anInteger <= 16rFFFF]) ifFalse: [^ self errorCantHold: anInteger].
	self
		nextPut: (anInteger bitAnd: 16rFF);
		nextPut: (anInteger bitShift: -8).

	^ anInteger.
!

padTo: anInteger put: anObject
	"copied from PositionableStream"

	| delta |

	delta := anInteger - (self position \\ anInteger).
	^ delta = anInteger 
		ifTrue: [0]
		ifFalse: [self next: delta put: anObject].
!

print: anObject
	"copied from WriteStream"

	anObject printOn: self.!

reverseContents
	"copied from PositionableStream (well, actually from FileStream)"

	^ (self contents readStream) reverseContents.
!

space
	"copied from WriteStream"

	self nextPut: Character space.!

tab
	"copied from WriteStream"

	self nextPut: Character tab.!

tab: anInteger
	"copied from WriteStream"

	anInteger timesRepeat: [self tab].! !
!AbstractWriteStream categoriesFor: #atEnd!must implement!public!should override!streaming!testing! !
!AbstractWriteStream categoriesFor: #basicPrint:!printing!private! !
!AbstractWriteStream categoriesFor: #binaryStore:!binary filing!public!storing! !
!AbstractWriteStream categoriesFor: #close!operations!public!should override! !
!AbstractWriteStream categoriesFor: #comma!constants!public! !
!AbstractWriteStream categoriesFor: #contents!accessing!public!should override! !
!AbstractWriteStream categoriesFor: #cr!public!writing! !
!AbstractWriteStream categoriesFor: #crtab!public!writing! !
!AbstractWriteStream categoriesFor: #crtab:!public!writing! !
!AbstractWriteStream categoriesFor: #display:!printing!public! !
!AbstractWriteStream categoriesFor: #flush!operations!public!should override! !
!AbstractWriteStream categoriesFor: #isReadable!public!testing! !
!AbstractWriteStream categoriesFor: #isWriteable!public!testing! !
!AbstractWriteStream categoriesFor: #next!accessing!public! !
!AbstractWriteStream categoriesFor: #next:put:!public!should override!writing! !
!AbstractWriteStream categoriesFor: #next:putAll:startingAt:!public!should override!writing! !
!AbstractWriteStream categoriesFor: #nextBeDOUBLEPut:!binary filing!public! !
!AbstractWriteStream categoriesFor: #nextBeDWORDPut:!binary filing!public! !
!AbstractWriteStream categoriesFor: #nextBeFLOATPut:!binary filing!public! !
!AbstractWriteStream categoriesFor: #nextBeQWORDPut:!binary filing!public! !
!AbstractWriteStream categoriesFor: #nextBeSDWORDPut:!binary filing!public! !
!AbstractWriteStream categoriesFor: #nextBeSQWORDPut:!binary filing!public! !
!AbstractWriteStream categoriesFor: #nextBeSWORDPut:!binary filing!public! !
!AbstractWriteStream categoriesFor: #nextBeWORDPut:!binary filing!public! !
!AbstractWriteStream categoriesFor: #nextDOUBLEPut:!binary filing!public! !
!AbstractWriteStream categoriesFor: #nextDWORDPut:!binary filing!public! !
!AbstractWriteStream categoriesFor: #nextFLOATPut:!binary filing!public! !
!AbstractWriteStream categoriesFor: #nextPut:!must implement!public!writing! !
!AbstractWriteStream categoriesFor: #nextPutAll:!public!writing! !
!AbstractWriteStream categoriesFor: #nextQWORDPut:!binary filing!public! !
!AbstractWriteStream categoriesFor: #nextSDWORDPut:!binary filing!public! !
!AbstractWriteStream categoriesFor: #nextSQWORDPut:!binary filing!public! !
!AbstractWriteStream categoriesFor: #nextSWORDPut:!binary filing!public! !
!AbstractWriteStream categoriesFor: #nextWORDPut:!binary filing!public! !
!AbstractWriteStream categoriesFor: #padTo:put:!positioning!public! !
!AbstractWriteStream categoriesFor: #print:!printing!public! !
!AbstractWriteStream categoriesFor: #reverseContents!accessing!public! !
!AbstractWriteStream categoriesFor: #space!constants!public! !
!AbstractWriteStream categoriesFor: #tab!public!writing! !
!AbstractWriteStream categoriesFor: #tab:!public!writing! !

AbstractReadWriteStream guid: (GUID fromString: '{5087A17B-A43D-4CF5-A7AA-799B40063F5A}')!
AbstractReadWriteStream comment: 'Copyright © Chris Uppal, 2002.
chris.uppal@metagnostic.org'!
!AbstractReadWriteStream categoriesForClass!Unclassified! !
!AbstractReadWriteStream methodsFor!

contents
	"it's not clear that this is really part of the external contract of a readable stream
	abstraction.  It seems to be appropriate only to external iterators (which 'know' that
	there's an underlying container of some sort), however ANSI makes it a part of
	<sequencedStream>, and hence <ReadStream>, so we'll include it here"

	| pos answer |

	pos := self position.
	self reset.
	answer := self upToEnd.
	self position: pos.

	^ answer.!

do: a1Block
	"one of the root methods for <ReadStream>; optional because implementable in terms
	of #atEnd and #next"

	"default implementation is provided by superclass"
	^ super do: a1Block.!

isReadable
	"one of the root methods for <ReadStream>"

	^ true.!

next
	"one of the root methods for <ReadStream>"

	self subclassResponsibility.
!

next: anInteger into: aSequenceableCollection startingAt: anIndex
	"one of the root methods for <ReadStream>; optional because implementable in terms
	of #next"

	"default implementation is provided by superclass"
	^ super next: anInteger into: aSequenceableCollection startingAt: anIndex.!

nextBeDOUBLE
	"copied from PositionableStream"

	| bytes |

	bytes := (ByteArray new: 8)
			at: 8 put: (self next);
			at: 7 put: (self next);
			at: 6 put: (self next);
			at: 5 put: (self next);
			at: 4 put: (self next);
			at: 3 put: (self next);
			at: 2 put: (self next);
			at: 1 put: (self next);
			yourself.
	^ bytes doubleAtOffset: 0!

nextBeDWORD
	"copied from PositionableStream"

#CUadded.
	^ (((self next bitShift: 24)
		bitOr: (self next bitShift: 16))
			bitOr: (self next bitShift: 8))
				bitOr: self next.!

nextBeFLOAT
	"copied from PositionableStream"

	| bytes |

#CUadded.
	bytes := (ByteArray new: 4)
			at: 4 put: (self next);
			at: 3 put: (self next);
			at: 2 put: (self next);
			at: 1 put: (self next);
			yourself.
	^ bytes floatAtOffset: 0!

nextBeQWORD
	"copied from PositionableStream"

	| bytes |

#CUadded.
	bytes := (ByteArray new: 8)
			at: 8 put: (self next);
			at: 7 put: (self next);
			at: 6 put: (self next);
			at: 5 put: (self next);
			at: 4 put: (self next);
			at: 3 put: (self next);
			at: 2 put: (self next);
			at: 1 put: (self next);
			yourself.
	^ bytes qwordAtOffset: 0!

nextBeSDWORD
	"copied from PositionableStream"

	| bytes |

#CUadded.
	bytes := (ByteArray new: 4)
			at: 4 put: (self next);
			at: 3 put: (self next);
			at: 2 put: (self next);
			at: 1 put: (self next);
			yourself.
	^ bytes sdwordAtOffset: 0!

nextBeSQWORD
	"copied from PositionableStream"

	| bytes |

#CUadded.
	bytes := (ByteArray new: 8)
			at: 8 put: (self next);
			at: 7 put: (self next);
			at: 6 put: (self next);
			at: 5 put: (self next);
			at: 4 put: (self next);
			at: 3 put: (self next);
			at: 2 put: (self next);
			at: 1 put: (self next);
			yourself.
	^ bytes sqwordAtOffset: 0!

nextBeSWORD
	"copied from PositionableStream"

	| int |
#CUadded.

	int := self nextBeWORD.
	^ int >= 16r8000
		ifTrue: [int - 16r10000]
		ifFalse: [int].!

nextBeWORD
	"copied from PositionableStream"

#CUadded.
	^ (self next bitShift: 8) bitOr: self next.
!

nextDOUBLE
	"copied from PositionableStream"

	^ (self next: 8) doubleAtOffset: 0.!

nextDWORD
	"copied from PositionableStream"

	^ (self next: 4) dwordAtOffset: 0.!

nextFLOAT
	"copied from PositionableStream"

	^ (self next: 4) floatAtOffset: 0.!

nextInto: aSequenceableCollection
	"copied from PositionableStream"

	^ self next: aSequenceableCollection size into: aSequenceableCollection startingAt: 1.!

nextLine
	"copied from PositionableStream"

	| result eol1 eol2 |

	eol1 := String lineDelimiter first.
	eol2 := String lineDelimiter last.
	result := self upTo: eol1.
	[self atEnd or: [self peekFor: eol2]]
		whileFalse: [result := result, (String with: eol1), (self upTo: eol1)].

	^ result.
!

nextQWORD
	"copied from PositionableStream"

	^ (self next: 8) qwordAtOffset: 0.!

nextSDWORD
	"copied from PositionableStream"

	^ (self next: 4) sdwordAtOffset: 0.!

nextSQWORD
	"copied from PositionableStream"

	^ (self next: 8) sqwordAtOffset: 0.!

nextSWORD
	"copied from PositionableStream"

	^ (self next: 2) swordAtOffset: 0.!

nextWord
	"copied from SequencedStream"

	| wordStream element |

	self skipSeparators ifFalse: [^ nil].
	wordStream := self contentsSpecies writeStream: 32.
	[self atEnd] whileFalse:
		[element := self next.
		element isSeparator ifTrue: [^ wordStream contents].
		wordStream nextPut: element].

	^ wordStream contents.
!

nextWORD
	"copied from PositionableStream"

	^ (self next: 2) wordAtOffset: 0.!

peek
	"copied from PositionableStream"

	| anObject |

	self atEnd ifTrue: [^ nil].

	anObject := self next.
	self pop.

	^ anObject.!

peekFor: anObject
	"copied from PositionableStream"

	^ self peek = anObject
		ifTrue: [self next. true]
		ifFalse: [false].!

skipSeparators
	"copied from PositionableStream"

	^ self skipWhile: [:c | c isSeparator].!

skipWhile: a1Block
	"copied from PositionableStream"

	[self atEnd] whileFalse:
		[(a1Block value: self next) ifFalse: [self pop. ^ true]].

	^ false.
!

skipWhileMatchAll: aCollection
	"copied from PositionableStream"

	^ aCollection skipOver: self.! !
!AbstractReadWriteStream categoriesFor: #contents!accessing!public!should override! !
!AbstractReadWriteStream categoriesFor: #do:!enumerating!public!should override!streaming! !
!AbstractReadWriteStream categoriesFor: #isReadable!public!testing! !
!AbstractReadWriteStream categoriesFor: #next!accessing!enumerating!must implement!public!streaming! !
!AbstractReadWriteStream categoriesFor: #next:into:startingAt:!accessing!public!should override! !
!AbstractReadWriteStream categoriesFor: #nextBeDOUBLE!binary filing!public! !
!AbstractReadWriteStream categoriesFor: #nextBeDWORD!binary filing!public! !
!AbstractReadWriteStream categoriesFor: #nextBeFLOAT!binary filing!public! !
!AbstractReadWriteStream categoriesFor: #nextBeQWORD!binary filing!public! !
!AbstractReadWriteStream categoriesFor: #nextBeSDWORD!binary filing!public! !
!AbstractReadWriteStream categoriesFor: #nextBeSQWORD!binary filing!public! !
!AbstractReadWriteStream categoriesFor: #nextBeSWORD!binary filing!public! !
!AbstractReadWriteStream categoriesFor: #nextBeWORD!binary filing!public! !
!AbstractReadWriteStream categoriesFor: #nextDOUBLE!binary filing!public! !
!AbstractReadWriteStream categoriesFor: #nextDWORD!binary filing!public! !
!AbstractReadWriteStream categoriesFor: #nextFLOAT!binary filing!public! !
!AbstractReadWriteStream categoriesFor: #nextInto:!accessing!public! !
!AbstractReadWriteStream categoriesFor: #nextLine!accessing!public! !
!AbstractReadWriteStream categoriesFor: #nextQWORD!binary filing!public! !
!AbstractReadWriteStream categoriesFor: #nextSDWORD!binary filing!public! !
!AbstractReadWriteStream categoriesFor: #nextSQWORD!binary filing!public! !
!AbstractReadWriteStream categoriesFor: #nextSWORD!binary filing!public! !
!AbstractReadWriteStream categoriesFor: #nextWord!accessing!public! !
!AbstractReadWriteStream categoriesFor: #nextWORD!binary filing!public! !
!AbstractReadWriteStream categoriesFor: #peek!accessing!public! !
!AbstractReadWriteStream categoriesFor: #peekFor:!accessing!public! !
!AbstractReadWriteStream categoriesFor: #skipSeparators!positioning!public! !
!AbstractReadWriteStream categoriesFor: #skipWhile:!positioning!public! !
!AbstractReadWriteStream categoriesFor: #skipWhileMatchAll:!positioning!public! !

"Binary Globals"!

"Resources"!

