| package |
package := Package name: 'CU Varargs'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org

This package adds an ExternalStructure that represents a C "va_list" object.  See VaList class comment for more details.  Also the package contains an implementation of String>>sprintfWithArguments: that is build on top of the varargs stuff.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris


History:

2.00
-	Fixed silly sends of non-existant #nextInteger and #nextUnsignedInteger.

1.00
-	First release.
'.

package basicPackageVersion: '2.02'.


package classNames
	add: #VaList;
	yourself.

package methodNames
	add: #Boolean -> #addToVaList:;
	add: #Character -> #addToVaList:;
	add: #CRTLibrary -> #_vsnprintf:count:format:va_list:;
	add: #Integer -> #addToVaList:;
	add: #Number -> #addToVaList:;
	add: #Object -> #addToVaList:;
	add: #SequenceableCollection -> #asVaList;
	add: #String -> #sprintfWithArguments:;
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

ExternalStructure subclass: #VaList
	instanceVariableNames: 'nextOffset'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!Boolean methodsFor!

addToVaList: aVaList
	"Private -- add a representation of the receiver to aVaList.
	This is part of the implementation of SequenceableCollection>>asVaList.
	Since we can only make a best guess at what representation is
	most appropriate it may sometimes be better to build a VaList explicitly
	rather then using #asVaList"
#CUadded.

	aVaList addBoolean: self.! !
!Boolean categoriesFor: #addToVaList:!double dispatch!operations!private! !

!Character methodsFor!

addToVaList: aVaList
	"Private -- add a representation of the receiver to aVaList.
	This is part of the implementation of SequenceableCollection>>asVaList.
	Since we can only make a best guess at what representation is
	most appropriate it may sometimes be better to build a VaList explicitly
	rather then using #asVaList"
#CUadded.

	aVaList addChar: self.! !
!Character categoriesFor: #addToVaList:!double dispatch!operations!private! !

!CRTLibrary methodsFor!

_vsnprintf: buffer count: maxbuf format: format va_list: aVaList
	"Private - Write data formatted by the format string into the buffer; aVaList
	supplies the 'variable' arguments to the call"

	<cdecl: sdword _vsnprintf lpvoid sdword lpstr VaList*>
#CUadded.
	^self invalidCall! !
!CRTLibrary categoriesFor: #_vsnprintf:count:format:va_list:!CRT functions-stream I/O!CU-utils!private! !

!Integer methodsFor!

addToVaList: aVaList
	"Private -- add a representation of the receiver to aVaList.
	This is part of the implementation of SequenceableCollection>>asVaList.
	Since we can only make a best guess at what representation is
	most appropriate it may sometimes be better to build a VaList explicitly
	rather then using #asVaList.
	This is a particular problem for Integer since we can't know which of the
	{signed/unsigned} {short/int/long/__int64} representations is right"
#CUadded.

	aVaList addInt: self.! !
!Integer categoriesFor: #addToVaList:!double dispatch!operations!private! !

!Number methodsFor!

addToVaList: aVaList
	"Private -- add a representation of the receiver to aVaList.
	This is part of the implementation of SequenceableCollection>>asVaList.
	Since we can only make a best guess at what representation is
	most appropriate it may sometimes be better to build a VaList explicitly
	rather then using #asVaList"
#CUadded.

	aVaList addFloat: self asFloat.! !
!Number categoriesFor: #addToVaList:!double dispatch!operations!private! !

!Object methodsFor!

addToVaList: aVaList
	"Private -- add a representation of the receiver to aVaList.
	This is part of the implementation of SequenceableCollection>>asVaList.
	Since we can only make a best guess at what representation is
	most appropriate it may sometimes be better to build a VaList explicitly
	rather then using #asVaList"
#CUadded.

	aVaList addAddressOf: self.! !
!Object categoriesFor: #addToVaList:!double dispatch!operations!private! !

!SequenceableCollection methodsFor!

asVaList
	"Answers a VaList with the same contents as the receiver.  Note that this is not aways
	exacty what you might want since we can only ask our elements to add themselves
	to the VaList, and they may 'guess' wrong about what format to use -- e.g. BigInteger
	does not automatically add itself as a 64-bit field"

	| answer |
#CUadded.

	answer := VaList new: self size.
	self do: [:each | each addToVaList: answer].

	^ answer.! !
!SequenceableCollection categoriesFor: #asVaList!converting!public! !

!String methodsFor!

sprintfWithArguments: aSequenceableCollection
	"Answer a String which is a message formatted from the receiver (assumed to be a C-printf
	format String) with substituations from the given SequenceableCollection of arguments.

	Note: the argument can be a previously created VaList instead of a SequenceableCollection.

	E.g:

		format two Floats right-justfied in a field of width 12, with 2 places of precision, and
		forcing the use/non-use of scientific notation.
		'[%*.*f][%*.*e]' sprintfWithArguments: #(12 2 3.141592653 12 2 3.141592653)

		format an integer right-justfied in a field of width 12, 0-padded to 5 digits.
		'[%*.*d]' sprintfWithArguments: #(12 5 33)

		format a string left-justfied in a field of width 30.
		'[%*s]' sprintfWithArguments: #(-30 'Hi there')

		format a string left-justfied in a field of width 10 and truncated to 3 letters.
		'[%-*.*s]' sprintfWithArguments: #(10 3 'Hi there')

		format 3 characters in fields of width 5.
		'[%5c][%5c][%5c]' sprintfWithArguments: #($A $B $C)

		format an integer as hex, 0-padded to 8 digits.
		'0x%.*X' sprintfWithArguments: #(8 47789)
	"

	| vaList n crt buffer size  |
#CUadded.

	crt := CRTLibrary default.
	vaList := aSequenceableCollection asVaList.
	size := self size + 128.	"not enough info to make a sensible guess"

	[buffer := String new: size.
	n := crt _vsnprintf: buffer count: size format: self va_list: vaList.
	n < 0]
		whileTrue: [size := size * 2].

	^ buffer copyFrom: 1 to: n.! !
!String categoriesFor: #sprintfWithArguments:!printing!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

VaList guid: (GUID fromString: '{0A3CAF07-4414-49A6-886B-0DAB5B5C2586}')!
VaList comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org

A VaList represents the "object" known in C as a ''va_list'' (see <stdarg.h>).

Many C functions take a variable number of parameters, for instance printf() does.  Using printf() as an example, the C delaration of printf() is something like:

	extern int printf(char *, ...);

where the ... means that it takes a variable number of extra arguments after the format specifier.  Dolphin''s external interfacing provides no  general way of calling such functions (the best you can do is have a number of different wrappers that each have a different number of arguments).  This class is intended to help with that problem.

The normal implementation of a function like printf() is to use the standard C feature called varargs, and the implementation of printf() might look something like:

	#include <stdarg.h>

	int
	printf(char *format, ...)
	{
		int retval;
		va_list args;

		va_start(args, format);
		retval = vprintf(format, args);
		va_end(args);

		return retval;
	}

Here the programmer uses <stdargs.h> to get a va_list structure that can be passed to the vprinf() function.  The important thing is that vprintf() takes a *fixed* number of arguments (one of which is a va_list), and so if we can fake the memory structure of a va_list in Dolphin, then we can call that function.  That wouldn''t be a lot of use in general except that nearly every programmer who writes varags functions follows the above pattern, with two functions: function(args, ...) and vfunction(args, va_list) where the latter is the implementation of the former.  Since the "v" functions (they nearly always are given names starting with ''v'') are useful in their own right, most APIs that use varargs functions also expose the "v" function equivalents.

VaList is intended to help build (or decode) such structures.  It has the usual two faces of an ExternalStructure.  In one role it stands for an externally defined va_list (presumably passed as the argument to some varags callback).  In this role it is unusual for an ExternalStructure in that it does *NOT KNOW HOW BIG IT IS*.  It is your responsibility to use some other indicator (such as a printf()-style format string) to "know" what it should contain and not run off the end.  In the other role, it wraps a chunk of Dolphin memory, and you build up a structure in that.

The way it is used is rather Stream-like.  It maintains a current "position" (like a pointer) and increments that as you either read elements from it, or add elements to it.  The pointer is actually the index of the next 32-bit word to be read or written (we use 32-bits because C va_lists are built with 32-bits for each element -- except floats and doubles which are both represented as 64-bit).

If you are building a VaList then you should try to give a reasonable estimate of how big it will eventually grow in the constructor (c.f. OrderedCollection); but the VaList will grow automatically if necessary.  Note that it does no great harm to use a VaList that is longer than is needed.

If you are reading an externally-supplied VaList, then be very careful -- you are messing around at the level of C pointers, and all sorts of Bad Things can happen if you screw up.

NB: when declaring a VaList in an external method''s external descriptor, it seems that you have to specify it as "VaList*" rather than "VaList".  I don''t know why.

See the implementation of String>>sprintfWithArguments: for an example of how to use VaLists.

	-- chris'!
!VaList categoriesForClass!Unclassified! !
!VaList methodsFor!

addAddressOf: anObject
	"add the in-memory address of anObject to our list, expanding if necessary.
	NB: be very carefull with this; if anObject is moved or garbage collected while
	this VaList is still in use, then Bad Things will happen.  Note that adding the
	address to the VaList does not itself protect anObject from GC"

	self addUnsignedInt: anObject yourAddress.!

addBoolean: aBool
	"add the C representation of aBoolean our list, expanding if necessary"

	"in C booleans are ints"
	self addInt: aBool asParameter.!

addChar: aCharacter
	"add a 'char' to our list, expanding if necessary"

	"chars in a C va_list are represented as 32-bit ints (we don't care about sign)"
	self addInt: (aCharacter codePoint).!

addDouble: aFloat
	"add a 'double' to our list, expanding if necessary"

	self ensureExtraCapactity: 2.
	bytes doubleAtOffset: nextOffset put: aFloat.
	self incrementPositionBy: 2.
!

addFloat: aFloat
	"add a 'float' to our list, expanding if necessary"

	"floats are promoted to double in a C va_list"
	self addDouble: aFloat.!

addInt: anInteger
	"add a (signed) 'int' to our list, expanding if necessary"

	self ensureExtraCapactity: 1.
	bytes sdwordAtOffset: nextOffset put: anInteger.
	self incrementPositionBy: 1.!

addInt64: anInteger
	"add a (signed) '__int64' to our list, expanding if necessary"

	self ensureExtraCapactity: 2.
	bytes sqwordAtOffset: nextOffset put: anInteger.
	self incrementPositionBy: 2.!

addLong: anInteger
	"add a (signed) 'long' to our list, expanding if necessary"

	"on Windows, ints are longs"
	self addInt: anInteger.!

addShort: anInteger
	"add a (signed) 'short' to our list, expanding if necessary"

	"shorts in a C va_list are represented as 32-bit unsigned ints"
	self addInt: anInteger.!

addSignedChar: aCharacter
	"add a 'signed char' to our list, expanding if necessary"

	"chars in a C va_list are represented as 32-bit ints (we don't care about sign)"
	self addInt: (aCharacter codePoint).!

addUnsignedChar: aCharacter
	"add an 'unsigned char' to our list, expanding if necessary"

	"chars in a C va_list are represented as 32-bit ints (we don't care about sign)"
	self addInt: (aCharacter codePoint).!

addUnsignedInt: anInteger
	"add an 'unsigned int' to our list, expanding if necessary"

	self ensureExtraCapactity: 1.
	bytes dwordAtOffset: nextOffset put: anInteger.
	self incrementPositionBy: 1.
!

addUnsignedInt64: anInteger
	"add an 'unsigned __int64' to our list, expanding if necessary"

	self ensureExtraCapactity: 2.
	bytes qwordAtOffset: nextOffset put: anInteger.
	self incrementPositionBy: 2.
!

addUnsignedLong: anInteger
	"add an 'unsigned long' to our list, expanding if necessary"

	"on Windows, ints are longs"
	self addUnsignedInt: anInteger.!

addUnsignedShort: anInteger
	"add an 'unsigned short' to our list, expanding if necessary"

	"unsigned shorts in a C va_list are represented as 32-bit unsigned ints"
	self addUnsignedInt: anInteger.!

asVaList
	"answer a VaList with the same contents as this collection.  This is implemented here
	so that VaLists can be passed to methods that expect to get SequencedCollections that
	they then convert to VaLists"

	"we are already a VaList"
	^ self.!

byteSize
	"answer the number of bytes we hold. This is 0 for an externally
	supplied instance"

	"overridden since we are not a fixed-size structure"
	^ self bytes size.!

capacity
	"answer how many 32-bit slots-worth of data we currently have space allocated
	to hold.
	If this is VaList refers to externally supplied memory (e.g. provided by a callback)
	then this measure is meaningless, and we answer 0"

	^ self byteSize // 4.!

capacity: anInteger
	"set how many 32-bit slots-worth of data we currently have space allocated
	to hold.
	If this is VaList refers to externally supplied memory (e.g. provided by a callback)
	then using this method will almost certainly not be  good idea!!"

	bytes resize: anInteger * 4.

	nextOffset > bytes size ifTrue: [nextOffset := bytes size].!

ensureCapacity: anInteger
	"ensure that we have the capacity to hold a total of at least anInteger 32-bit elements"

	self capacity < anInteger ifTrue: [self capacity: anInteger + self class capacityIncrement].
!

ensureExtraCapactity: anInteger
	"ensure that we have the capacity to add at least anInteger more 32-bit elements without	
	resizing"

	self ensureCapacity: anInteger + self position - 1.!

ensureInitialized
	"private -- ensure that we are initialized properly, this is needed
	since we may be created in an uninitialized form directly by the
	VM"

	nextOffset isNil ifTrue: [nextOffset := 0].
!

incrementPositionBy: anInteger
	"increment the index of the next 32-bit word to be read from or
	written to this list"

	nextOffset := nextOffset + (anInteger * 4).!

nextAddress
	"interpret the next 32-bit word in the list as a 'void*' (address) and
	answer that.
	Answers an instance of ExternalAddress"

	^ self nextInstanceOfClass: ExternalAddress.!

nextBoolean
	"interpret the next 32-bit word in the list as a Boolean and
	answer that"

	"use C interpretation where anything exept 0 is true"
	^ self nextInt ~= 0.!

nextChar
	"interpret the next 32-bit word in the list as a 'char' and
	answer that"

	^ Character codePoint: self nextInt.!

nextDouble
	"interpret the next two 32-bit words in the list as an 'double' and
	answer that"

	| answer |

	self ensureInitialized.
	answer := bytes doubleAtOffset: nextOffset.
	self incrementPositionBy: 2.

	^ answer.
!

nextFloat
	"interpret the next two 32-bit words in the list as an 'float' and
	answer that
	NB: we read 64-bits because C encodes floats as doubles in this context"

	^ self nextDouble.!

nextInstanceOfClass: aClass
	"interpret the next 32-bit word in the list as the address of
	an instance of aClass and answer that.
	aClass must have a factory method called #fromAddress: that takes an Integer
	as parameter (e.g. String does)"

	^ aClass fromAddress: self nextUnsignedInt.!

nextInt
	"interpret the next 32-bit word in the list as an 'int' and
	answer that"

	| answer |

	self ensureInitialized.
	answer := bytes sdwordAtOffset: nextOffset.
	self incrementPositionBy: 1.

	^ answer.
!

nextInt64
	"interpret the next two 32-bit words in the list as an '__int64' and
	answer that"

	| answer |

	self ensureInitialized.
	answer := bytes sqwordAtOffset: nextOffset.
	self incrementPositionBy: 2.

	^ answer.
!

nextLong
	"interpret the next 32-bit word in the list as an 'long' and
	answer that"

	"Windows long is the same as int"
	^ self nextInt.!

nextShort
	"interpret the next 32-bit word in the list as a 'short' and
	answer that"

	^ self nextInt.!

nextSignedChar
	"interpret the next 32-bit word in the list as a 'signed char' and
	answer that"

	^ Character codePoint: self nextInt.!

nextUnsignedChar
	"interpret the next 32-bit word in the list as an 'unsigned char' and
	answer that"

	^ Character codePoint: self nextUnsignedInt.!

nextUnsignedInt
	"interpret the next 32-bit word in the list as an 'unsigned int' and
	answer that"

	| answer |

	self ensureInitialized.
	answer := bytes dwordAtOffset: nextOffset.
	self incrementPositionBy: 1.

	^ answer.
!

nextUnsignedInt64
	"interpret the next two 32-bit words in the list as an 'unsigned __int64' and
	answer that"

	| answer |

	self ensureInitialized.
	answer := bytes qwordAtOffset: nextOffset.
	self incrementPositionBy: 2.

	^ answer.
!

nextUnsignedLong
	"interpret the next 32-bit word in the list as an 'unsigned long' and
	answer that"

	"Windows long is the same as int"
	^ self nextUnsignedInt.!

nextUnsignedShort
	"interpret the next 32-bit word in the list as an 'unsigned short' and
	answer that"

	^ self nextUnsignedInt.!

position
	"answer the index of the next 32-bit word to be read from or
	written to this list"

	self ensureInitialized.

	"internally we use byte offsets so we have to convert"
	^ (nextOffset // 4) + 1.!

position: anInteger
	"set the index of the next 32-bit word to be read from or
	written to this list"

	"internally we use byte offsets so we have to convert"
	nextOffset := anInteger - 1 * 4.! !
!VaList categoriesFor: #addAddressOf:!adding!public! !
!VaList categoriesFor: #addBoolean:!adding!public! !
!VaList categoriesFor: #addChar:!adding!public! !
!VaList categoriesFor: #addDouble:!adding!public! !
!VaList categoriesFor: #addFloat:!adding!public! !
!VaList categoriesFor: #addInt:!adding!public! !
!VaList categoriesFor: #addInt64:!adding!public! !
!VaList categoriesFor: #addLong:!adding!public! !
!VaList categoriesFor: #addShort:!adding!public! !
!VaList categoriesFor: #addSignedChar:!adding!public! !
!VaList categoriesFor: #addUnsignedChar:!adding!public! !
!VaList categoriesFor: #addUnsignedInt:!adding!public! !
!VaList categoriesFor: #addUnsignedInt64:!adding!public! !
!VaList categoriesFor: #addUnsignedLong:!adding!public! !
!VaList categoriesFor: #addUnsignedShort:!adding!public! !
!VaList categoriesFor: #asVaList!converting!public! !
!VaList categoriesFor: #byteSize!accessing!public! !
!VaList categoriesFor: #capacity!accessing!public! !
!VaList categoriesFor: #capacity:!accessing!public! !
!VaList categoriesFor: #ensureCapacity:!operations!public! !
!VaList categoriesFor: #ensureExtraCapactity:!operations!public! !
!VaList categoriesFor: #ensureInitialized!operations!private! !
!VaList categoriesFor: #incrementPositionBy:!positioning!public! !
!VaList categoriesFor: #nextAddress!public!reading! !
!VaList categoriesFor: #nextBoolean!public!reading! !
!VaList categoriesFor: #nextChar!public!reading! !
!VaList categoriesFor: #nextDouble!public!reading! !
!VaList categoriesFor: #nextFloat!public!reading! !
!VaList categoriesFor: #nextInstanceOfClass:!public!reading! !
!VaList categoriesFor: #nextInt!public!reading! !
!VaList categoriesFor: #nextInt64!public!reading! !
!VaList categoriesFor: #nextLong!public!reading! !
!VaList categoriesFor: #nextShort!public!reading! !
!VaList categoriesFor: #nextSignedChar!public!reading! !
!VaList categoriesFor: #nextUnsignedChar!public!reading! !
!VaList categoriesFor: #nextUnsignedInt!public!reading! !
!VaList categoriesFor: #nextUnsignedInt64!public!reading! !
!VaList categoriesFor: #nextUnsignedLong!public!reading! !
!VaList categoriesFor: #nextUnsignedShort!public!reading! !
!VaList categoriesFor: #position!accessing!positioning!public! !
!VaList categoriesFor: #position:!accessing!positioning!public! !

!VaList class methodsFor!

capacityIncrement
	"answer how many extra 32-bit slots we add each time we
	expand ourself"

	^ 3.!

defaultCapacity
	"answer how many 32-bit slots-worth of capacity to given instances by
	default"

	^ 3.!

defineFields
	"since VaLists do not correspond to classic C structure, we don't actually define any fields"

	"no nothing"!

new: anInteger
	"answer a new instance initialized with the capacity to hold anInteger 32-bit words of
	data without having to grow itself"

	^ super new: anInteger * 4.!

newBuffer
	"answer a new instance containing its own buffer and with the default
	capacity"

	^ self new: self defaultCapacity.! !
!VaList class categoriesFor: #capacityIncrement!constants!public! !
!VaList class categoriesFor: #defaultCapacity!constants!public! !
!VaList class categoriesFor: #defineFields!constants!public! !
!VaList class categoriesFor: #new:!instance creation!public! !
!VaList class categoriesFor: #newBuffer!instance creation!public! !

"Binary Globals"!

"Resources"!

