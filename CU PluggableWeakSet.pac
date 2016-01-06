| package |
package := Package name: 'CU PluggableWeakSet'.
package paxVersion: 0;
	basicComment: 'PluggableWeakSet is a weak set that is pluggable.  Doh!!

This is *not* "Copyright © Chris Uppal, 2002." since I didn''t write it, it was assembled (all but one method) by drag-n-drop from PluggableSet and WeakSet.

	-- chris	(chris.uppal@metagnostic.org)'.

package basicPackageVersion: '0.0002 (unpublished)'.


package classNames
	add: #PluggableWeakSet;
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

WeakSet variableSubclass: #PluggableWeakSet
	instanceVariableNames: 'searchPolicy'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

PluggableWeakSet guid: (GUID fromString: '{B1E354E0-F350-406B-A83B-FAC2ACAFAB65}')!
PluggableWeakSet comment: 'PluggableWeakSet is a weak set that is pluggable.  Well, what *else* did you expect it to be!!

This is *not* "Copyright © Chris Uppal, 2002." since I didn''t write it, it''s assembled by drag-n-drop from PluggableSet and WeakSet.

	-- chris	(chris.uppal@metagnostic.org)'!
!PluggableWeakSet categoriesForClass!Unclassified! !
!PluggableWeakSet methodsFor!

copyEmpty: anInteger
	"Private - Answer an empty copy of the receiver, with enough space for anInteger
	number of elements. 
	Implementation Note: We must override in order to preserve the searchPolicy."

	^(super copyEmpty: anInteger) setSearchPolicy: self searchPolicy!

findElementOrNil: anObject
	"Private - Answer the index of the given object in the receiver, or, if not found,
	the index of the first empty slot including and after that to which the object hashes.
	A pluggable <searchPolicy> is used for key comparisons and hashing."

	| capacity index element corpse |
	capacity := self basicSize.
	index := self hash: anObject max: capacity.
	corpse := DeadObject current.

	[(element := self basicAt: index) isNil
		or: [element = corpse
			or: [searchPolicy compare: element with: anObject]]]
				whileFalse: [index := index \\ capacity + 1].
	^index!

hash: anObject max: anInteger
	^searchPolicy hash: anObject max: anInteger!

initialize
	"Private - Instance variable initialization. The tally records the number of elements."

	super initialize.
	searchPolicy := self class defaultSearchPolicy
!

newSelection
	"Private - Answer a new empty collection like the receiver to 
	contain a selection of the receiver's elements."

	^self class searchPolicy: self searchPolicy!

preResize: newMe
	"This message is sent by the receiver when resizing, before the
	receiver's elements are added to newMe. We must assign across the
	search policy."

	newMe setSearchPolicy: self searchPolicy!

searchPolicy
	"Answer the receiver's <searchPolicy>."

	^searchPolicy!

searchPolicy: aSearchPolicy
	"Set the receiver's <searchPolicy>."

	self setSearchPolicy: aSearchPolicy.
	self isEmpty ifFalse: [self rehash]!

setSearchPolicy: aSearchPolicy
	"Private - Set the receiver's <searchPolicy>. Answer the receiver."

	searchPolicy := aSearchPolicy.
	^self! !
!PluggableWeakSet categoriesFor: #copyEmpty:!copying!private! !
!PluggableWeakSet categoriesFor: #findElementOrNil:!private!searching! !
!PluggableWeakSet categoriesFor: #hash:max:!private!searching! !
!PluggableWeakSet categoriesFor: #initialize!initializing!private! !
!PluggableWeakSet categoriesFor: #newSelection!enumerating!private! !
!PluggableWeakSet categoriesFor: #preResize:!adding!public! !
!PluggableWeakSet categoriesFor: #searchPolicy!accessing!public! !
!PluggableWeakSet categoriesFor: #searchPolicy:!accessing!public! !
!PluggableWeakSet categoriesFor: #setSearchPolicy:!accessing!private! !

!PluggableWeakSet class methodsFor!

defaultSearchPolicy
	"Answer the default <searchPolicy> used by instances of the receiver."

	^SearchPolicy equality!

initialize
	"Initialize the receiver.
	Bereavement notifications are required by the receiver's instances to
	effect repairs."

	self makeMourner!

new: anInteger searchPolicy: aSearchPolicy
	"Answer a new instance of the receiver with an initial capacity of at least 
	<integer> count elements (i.e. the size is a hint), and with the specified <searchPolicy>."

	^(self new: anInteger) setSearchPolicy: aSearchPolicy!

searchPolicy: policy
	"Answer a new, empty, instance of the receiver with the specified <searchPolicy>."

	^self new setSearchPolicy: policy! !
!PluggableWeakSet class categoriesFor: #defaultSearchPolicy!constants!public! !
!PluggableWeakSet class categoriesFor: #initialize!development!initializing!public! !
!PluggableWeakSet class categoriesFor: #new:searchPolicy:!instance creation!public! !
!PluggableWeakSet class categoriesFor: #searchPolicy:!instance creation!public! !

"Binary Globals"!

"Resources"!

