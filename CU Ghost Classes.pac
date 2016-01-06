| package |
package := Package name: 'CU Ghost Classes'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2002.
chris.uppal@metagnostic.org

A "ghost class" is an ephemeral, dynamically-generated, class that is not linked into the browsable hierarchy.  It contains "ghost methods" which are normal methods except that:
-	their source is not saved in the .chg file (and may not be saved at all)
-	they can, optionally, be created with a Symbol->Object map.  Occurences of the Symbol in the source are converted to hardwired references to the given Object in the compiled method.  (Sort of like the ##(...) mechanism).

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris'.

package basicPackageVersion: '1.01'.


package classNames
	add: #GhostMethod;
	yourself.

package methodNames
	add: #Class -> #ghostClassRoot;
	add: #Class -> #isGhostClass;
	add: #Class -> #makeGhostClass:;
	add: #ClassDescription -> #compileGhostMethod:;
	add: #ClassDescription -> #compileGhostMethod:map:;
	add: #Metaclass -> #isGhostClass;
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

CompiledMethod variableSubclass: #GhostMethod
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'byteCodesCache'!

"Global Aliases"!


"Loose Methods"!

!Class methodsFor!

ghostClassRoot
	"If the receiver is a ghost class then answer the 'real' class from which it was derived,
	otherwise answer the receiver."

#CUadded.

	^ self isGhostClass
		ifTrue: [self superclass ghostClassRoot]
		ifFalse: [self].!

isGhostClass
	"Answer whether the receiver is a 'ghost class'.  None of the normal Smalltalk classes
	are, only those generated on-the-fly by #makeGhostClass:, and they never appear
	in the class browsers etc."

#CUadded.

	^ false.!

makeGhostClass: aString
	"Answer a new subclass of the receiver which is not installed as a global variable, nor
	linked into the class hierarchy proper.  It will consider its name to be aString, but is -- of
	course -- not accessible under that name  It will have no instance methods installed initially,
	and only:
		isGhostClass ^ true.
	on the class side"

	| ghost |

#CUadded.

	ghost := self anonymousSubclass: aString.

	ghost class compileGhostMethod: 'isGhostClass ^ true.'.

	^ ghost.! !
!Class categoriesFor: #ghostClassRoot!ghost classes!public!testing! !
!Class categoriesFor: #isGhostClass!ghost classes!public!testing! !
!Class categoriesFor: #makeGhostClass:!ghost classes!public! !

!ClassDescription methodsFor!

compileGhostMethod: aString
	"Private - Compile aString and enter a GhostMethod corresponding to the result to our
	method dictionary.
	The important thing about ghost methods is that they consume few system resources -- they
	are not written to the change log, and share duplicate byte code arrays.
	Answers the ghost method or nil if the compilation failed."

#CUadded.

	^ self compileGhostMethod: aString map: nil.!

compileGhostMethod: aString map: aDictionary
	"Private - Compile aString and enter a GhostMethod corresponding to the result to our
	method dictionary.
	The map is a Dictionary mapping literals in the generated method's to replacement values.
	The important thing about ghost methods is that they consume few system resources -- they
	are not written to the change log, and share duplicate byte code arrays.
	Answers the ghost method or nil if the compilation failed."

	| method |
#CUadded.

	[method := self compilerClass compile: aString in: self]
		on: CompilerErrorNotification
		do: [:err | ^ nil].

	method := GhostMethod newAsCopyOf: method map: aDictionary.

	self addSelector: method selector withMethod: method.

	^ method.! !
!ClassDescription categoriesFor: #compileGhostMethod:!ghost classes!object methods!public! !
!ClassDescription categoriesFor: #compileGhostMethod:map:!ghost classes!object methods!public! !

!Metaclass methodsFor!

isGhostClass
	"Answer whether the receiver is a 'ghost class'.  None of the normal Smalltalk classes
	are, only those generated on-the-fly by #makeGhostClass:, and they never appear
	in the class browsers etc."

#CUadded.

	^ self instanceClass isGhostClass.! !
!Metaclass categoriesFor: #isGhostClass!ghost classes!public!testing! !

"End of package definition"!

"Source Globals"!

"Classes"!

GhostMethod guid: (GUID fromString: '{00B9D154-1C40-42FC-AB4E-634612085392}')!
GhostMethod comment: 'Copyright © Chris Uppal, 2002.
chris.uppal@metagnostic.org'!
!GhostMethod categoriesForClass!Unclassified! !
!GhostMethod methodsFor!

asDebugMethod
	"private -- overridden to copy across the massaged literals too.
	NB: this is not called if we don't save (or regenerate on demand)
	the source from which we were compiled"

	| debugMethod |

	self isDebugMethod ifTrue: [^self].

	debugMethod := super asDebugMethod.

	"ensure that replaced literals are copied across"
	1 to: self literalCount do: [:i | debugMethod literalAt: i put: (self literalAt: i)].

	^ debugMethod.!

byteCodes: anObject
	"private -- set the byte code object.  This is duplicated here since the superclass def is part of the
	lagoon package and won't be present on all systems"

	byteCodes := anObject.
! !
!GhostMethod categoriesFor: #asDebugMethod!development!private! !
!GhostMethod categoriesFor: #byteCodes:!accessing!private! !

!GhostMethod class methodsFor!

cacheByteCodes: aByteArrayOrInteger
	"private -- look for a previous version of the given byte code array and, if we've got
	an equivalent object in our cache already, answer it.  Otherwise add the given object
	and answer that"

	"nothing to be gained by cacheing these"
	aByteArrayOrInteger isImmediate ifTrue: [^ aByteArrayOrInteger].

	byteCodesCache isNil ifTrue: [byteCodesCache := WeakSet new].

	^ byteCodesCache find: aByteArrayOrInteger ifAbsent: [byteCodesCache add: aByteArrayOrInteger].

!

newAsCopyOf: aCompiledMethod map: aDictionary
	"answer a new ghost method which is otherwise a duplicate
	of aCompiledMethod, but with any literals that appear as keys
	in aDictionary replaced by the corresponding values"

	| new |

	new :=  aCompiledMethod shallowCopy.
	
	new becomeA: self.

	"try to maximise sharing of bytecodes"
	new byteCodes: (self cacheByteCodes: aCompiledMethod byteCodes).

	"replace any literals that need it"
	aDictionary isNil ifFalse:
		[1 to: new literalCount do:
			[:i || literal replacement |
			literal := new literalAt: i.
			replacement := aDictionary at: literal ifAbsent: [literal].
			replacement == literal ifFalse: [new literalAt: i put: replacement]]].

	^ new.! !
!GhostMethod class categoriesFor: #cacheByteCodes:!helpers!private! !
!GhostMethod class categoriesFor: #newAsCopyOf:map:!instance creation!public! !

"Binary Globals"!

"Resources"!

