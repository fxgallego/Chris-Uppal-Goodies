| package |
package := Package name: 'CU Always'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2001-2005.
chris.uppal@metagnostic.org

Simple class that implements the <*Valuable> protocols with objects that always answer the same value.  Includes some "Singleton"-like special cases.  The main point is that I think

	blahblah setCondition: Always true.

reads better than:

	blahblah setCondition: [:it | true].

However it''s a potentially important point that in current Dolphin, a block captured in a long-lived object will hold on to references to its last parameters.  An object like

	Always true.

does not.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris


History:

3.00
-	Added  a sensible #printOn: method.

2.00
-	Added #empty (yeah, I know -- not a very big deal...).

1.00
-	First release.
'.

package basicPackageVersion: '3.01'.


package classNames
	add: #Always;
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

Object subclass: #Always
	instanceVariableNames: 'value'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'alwaysTrue alwaysFalse alwaysNil alwaysOne alwaysZero alwaysEmpty'!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

Always guid: (GUID fromString: '{E20542D3-53C3-11D3-8725-BC9EBD3E4405}')!
Always comment: 'Copyright © Chris Uppal, 2001-2005.
chris.uppal@metagnostic.org

Class that can be used instead of constant-valued BlockClosures.

There are a few predefined "constants":
	Always true
	Always false
	Always nil
	etc.
and instances can also be created that always answer other values:, e.g.
	Always answer: 42.'!
!Always categoriesForClass!No category! !
!Always methodsFor!

initialize: anObject
	"private -- set up instance variables"

	value := anObject.!

printOn: aStream
	"append a developer friendly representation of ourself to the given stream.
	This implementation allows the reciever to be recreated (e.g. in an Inspector)"

	aStream display: self class; space.

	"ugly hack"
	#( #true #false #nil #one #zero #empty )
		do: [:each | self == (self class perform: each) ifTrue:
				[aStream display: each.
				^ self]].

	aStream
		display: 'answer: ';
		print: value.
!

value
	"allows the receiver to be used wherever a niladic BlockClosure is used
	to answer a constant"

	^ value.!

value: anObject
	"allows the receiver to be used wherever a monadic BlockClosure is used
	to answer a constant"

	^ self value.!

value: anObject1 value: anObject2
	"allows the receiver to be used wherever a diadic BlockClosure is used
	to answer a constant"

	^ self value.!

value: anObject1 value: anObject2 value: anObject3
	"allows the receiver to be used wherever a triadic BlockClosure is used
	to answer a constant"

	^ self value.!

valueWithArguments: argumentArray
	"allows the receiver to be used wherever an N-adic BlockClosure is used
	to answer a constant"

	^ self value.! !
!Always categoriesFor: #initialize:!initializing!private! !
!Always categoriesFor: #printOn:!printing!public! !
!Always categoriesFor: #value!evaluating!public! !
!Always categoriesFor: #value:!evaluating!public! !
!Always categoriesFor: #value:value:!evaluating!public! !
!Always categoriesFor: #value:value:value:!evaluating!public! !
!Always categoriesFor: #valueWithArguments:!evaluating!public! !

!Always class methodsFor!

answer: anObject
	"answer an instance which will always reply anObject to any of the X-adic valuable messages"

	^ (self new)
		initialize: anObject;
		yourself.!

empty
	"answer the singleton instance that always answers an empty Array"

	^ alwaysEmpty!

false
	"answer the singleton instance that always answers false"

	^ alwaysFalse.!

ignoreIt
	"e.g.

		[...some expression...]
			on: Error
			do: Always ignoreIt.
	"

	^ alwaysNil.!

initialize
	"private -- set up the class on loading.
		self initialize.
	"

	alwaysTrue := Always answer: true.
	alwaysFalse := Always answer: false.
	alwaysNil := Always answer: nil.
	alwaysOne := Always answer: 1.
	alwaysZero := Always answer: 0.
	alwaysEmpty := Always answer: #().
!

nil
	"answer the singleton instance that always answers nil"

	^ alwaysNil.!

one
	"answer the singleton instance that always answers 1"

	^ alwaysOne.!

true
	"answer the singleton instance that always answers true"

	^ alwaysTrue.!

uninitialize
	"private -- tear down the class before unloading.
		self uninitialize.
	"

	alwaysTrue := alwaysFalse := alwaysNil := alwaysOne := alwaysZero := nil.
!

zero
	"answer the singleton instance that always answers 0"

	^ alwaysZero.
! !
!Always class categoriesFor: #answer:!instance creation!public! !
!Always class categoriesFor: #empty!constants!public! !
!Always class categoriesFor: #false!constants!public! !
!Always class categoriesFor: #ignoreIt!constants!public! !
!Always class categoriesFor: #initialize!initializing!private! !
!Always class categoriesFor: #nil!constants!public! !
!Always class categoriesFor: #one!constants!public! !
!Always class categoriesFor: #true!constants!public! !
!Always class categoriesFor: #uninitialize!initializing!private! !
!Always class categoriesFor: #zero!constants!public! !

"Binary Globals"!

"Resources"!

