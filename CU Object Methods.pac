| package |
package := Package name: 'CU Object Methods'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2001-2005.
chris.uppal@metagnostic.org

Little framework for defining and managing object-specific methods.

Example:

	"start with this:"
	s := String fromString: ''Hi there!!''.

	"now try these; all should be normal"
	s class.			"should be String" 
	s basicClass.		"should be String" 
	s isSpecialized.		"should be false" 
	s specializedSelectors.	"should be empty" 
	s asLowercase.		"should be ''hi there!!''" 

	"now add a specialised method to this one object:"
	s addSpecializedMethod: ''asLowercase ^super asUppercase''.

	"then try:"
	s asLowercase.		"should be ''HI THERE!!''" 

	"and note that:"
	s class.			"should be String" 
	s basicClass.		"should be String+" 
	s isSpecialized.		"should be true" 
	s specializedSelectors.	"should be {#isSpecialized #asLowercase #class}" 

	"now remove the specialization:"
	s removeSpecializedSelector: #asLowercase.

	"and everything is normal again"
	s class.			"should be String" 
	s basicClass.		"should be String" 
	s isSpecialized.		"should be false" 
	s specializedSelectors.	"should be empty" 
	s asLowercase.		"should be ''hi there!!''" 


The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.


'.

package basicPackageVersion: '1.00'.


package methodNames
	add: #Class -> #makeSpecializedSubclass;
	add: #Object -> #addSpecializedMethod:;
	add: #Object -> #basicBeNotSpecialized;
	add: #Object -> #basicBeSpecialized;
	add: #Object -> #basicIsSpecializedOn:;
	add: #Object -> #basicRemoveSpecializedSelector:;
	add: #Object -> #basicSpecializedMethods;
	add: #Object -> #beNotSpecialized;
	add: #Object -> #beSpecialized;
	add: #Object -> #isSpecialized;
	add: #Object -> #isSpecializedOn:;
	add: #Object -> #removeSpecializedSelector:;
	add: #Object -> #specializedMethods;
	add: #Object -> #specializedSelectors;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'CU Anonymous Subclass';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Base\Dolphin';
	yourself).

package setManualPrerequisites: #(
	'CU Anonymous Subclass').

package!

"Class Definitions"!


"Global Aliases"!


"Loose Methods"!

!Class methodsFor!

makeSpecializedSubclass
	"Answer a new subclass of the receiver which is not installed as a global variable, nor
	linked into the class hierarchy proper.  It will have two methods installed initially:

		isSpecialized		^ true.
		class			^ <global class from which it was derived>.

	"

#CUadded.

	^ (self anonymousSubclass: (self name , '+'))
		basicCompile: 'isSpecialized ^true';
		basicCompile: ('class ^' , self name);
		yourself.! !
!Class categoriesFor: #makeSpecializedSubclass!anonymous subclassing!object methods!public! !

!Object methodsFor!

addSpecializedMethod: aString
	"Compile aString, which is expected to contain valid source for a method, into
	the receiver as a per-object method.
	Answers the new method (in case anyone cares)."

	| method |
#CUadded.

	"slightly convoluted expression so that we can avoid failed compilations leaving
	us specialised wrongly"

	self isSpecialized ifTrue: [^ self basicClass basicCompile: aString].

	[self beSpecialized.
	method := self addSpecializedMethod: aString]
		ifCurtailed: [self beNotSpecialized].

	^ method.!

basicBeNotSpecialized
	"Private - Ensure the receiver has no specialised private class (i.e. does not have per-object methods).
	This differs from #beNotSpecialized in that it assumes that the receiver is already specialized."
#CUadded.

	self becomeA: (self basicClass superclass).
!

basicBeSpecialized
	"Private - Ensure the receiver has a specialised private class (i.e. can have per-object methods).
	This differs from #beSpecialized in that it assumes that the receiver is not already specialized."
#CUadded.

	self becomeA: (self basicClass makeSpecializedSubclass).!

basicIsSpecializedOn: aSymbol
	"Private - Answer whether the receiver has a specialized method with the given selector.
	This differs from #isSpecializedOn: in that it assumes that the receiver
	is specialized."
#CUadded.

	^ self basicClass includesSelector: aSymbol.
!

basicRemoveSpecializedSelector: aSymbol
	"Private - Remove the specialized method with the given selector.
	This differs from #removeSpecializedMethod: in that it assumes that the receiver
	is specialized."
	| class dict |
#CUadded.

	"if someone removes #isSpecialized then all hell breaks loose..."
	self assert: [aSymbol ~~ #isSpecialized].

	class := self basicClass.
	dict := class methodDictionary.

	"NB: we don't use:
		class removeSelector: aSymbol.
	because that generates #methodRemoved: notifications"
	dict removeKey: aSymbol.
	class flushMethodCache.

	"if there's no further specializations, then remove the machinery"
	(dict keys - #(#isSpecialized #class)) isEmpty
		ifTrue: [self basicBeNotSpecialized].!

basicSpecializedMethods
	"Private - Answer an OrderedCollection of all the methods which are specialized on the receiver.
	This differs from #pecializedMethods in that it assumes that the receiver
	is specialized."
#CUadded.

	^ self basicClass methodDictionary values asOrderedCollection.
!

beNotSpecialized
	"Ensure the receiver has no specialised private class (i.e. does not have per-object methods)."
#CUadded.

	"we use a test rather than polymorphism to avoid cluttering the space of potential specialisations"
	self isSpecialized ifTrue: [self basicBeNotSpecialized].
!

beSpecialized
	"Ensure the receiver has a specialised private class (i.e. can have per-object methods)."
#CUadded.

	"we use a test rather than polymorphism to avoid cluttering the space of potential specialisations"
	self isSpecialized ifFalse: [self basicBeSpecialized].
!

isSpecialized
	"Answer whether the receiver has a specialised private class (i.e. can have per-object methods)."
#CUadded.

	"this method is always specialized in specialized objects to answer true"
	^ false.
!

isSpecializedOn: aSymbol
	"Answer whether the receiver has a specialised private class (i.e. can have per-object methods)."
#CUadded.

	"we use a test rather than polymorphism to avoid cluttering the space of potential specialisations"
	^ self isSpecialized
		ifTrue: [self basicIsSpecializedOn: aSymbol]
		ifFalse: [false].
!

removeSpecializedSelector: aSymbol
	"Remove the specialized method with the given selector."
#CUadded.

	"we use a test rather than polymorphism to avoid cluttering the space of potential specialisations"
	self isSpecialized
		ifTrue: [self basicRemoveSpecializedSelector: aSymbol]
		ifFalse: [self errorNotFound: aSymbol].!

specializedMethods
	"Answer an OrderedCollection of all the methods which are specialized on the receiver.
	Note that this will always include #class and #isSpecialized if we are indeed specialised."
#CUadded.

	"we use a test rather than polymorphism to avoid cluttering the space of potential specialisations"
	^ self isSpecialized
		ifTrue: [self basicSpecializedMethods]
		ifFalse: [OrderedCollection new].
!

specializedSelectors
	"Answer an OrderedCollection of all the methods which are specialized on the receiver.
	Note that this will always include #class and #isSpecialized if we are indeed specialised."
#CUadded.

	^ self specializedMethods collect: [:each | each selector].
! !
!Object categoriesFor: #addSpecializedMethod:!object methods!public! !
!Object categoriesFor: #basicBeNotSpecialized!accessing!object methods!private! !
!Object categoriesFor: #basicBeSpecialized!accessing!object methods!private! !
!Object categoriesFor: #basicIsSpecializedOn:!object methods!private!testing! !
!Object categoriesFor: #basicRemoveSpecializedSelector:!object methods!private! !
!Object categoriesFor: #basicSpecializedMethods!object methods!private! !
!Object categoriesFor: #beNotSpecialized!accessing!object methods!public! !
!Object categoriesFor: #beSpecialized!accessing!object methods!public! !
!Object categoriesFor: #isSpecialized!object methods!public!testing! !
!Object categoriesFor: #isSpecializedOn:!object methods!public!testing! !
!Object categoriesFor: #removeSpecializedSelector:!object methods!public! !
!Object categoriesFor: #specializedMethods!object methods!public! !
!Object categoriesFor: #specializedSelectors!object methods!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

"Binary Globals"!

"Resources"!

