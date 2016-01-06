| package |
package := Package name: 'CU JNI'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2001-2005.
chris.uppal@metagnostic.org

Wrapper for the Java Native Interface library.

This package provides very little beyond raw access to the features of the library.

The only extensions are:
	- some methods to help with error checking (there are error checked versions of all the JNIEnv calls).
	- a means of keeping local and global object references distinct and able to release themselves (But are *not* finalisable).
	- a slightly easier way of building argument lists (JNIValueArray).
	- a dummy JNIEnv that can be used as a "deaf object" for less messy closedown.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris'.

package basicPackageVersion: '1.09'.


package classNames
	add: #DeadJNIEnv;
	add: #FunctionPointerField;
	add: #JavaVM;
	add: #JavaVMInitArgs;
	add: #JavaVMOption;
	add: #JDK1_1InitArgs;
	add: #JNIArray;
	add: #JNIArrayG;
	add: #JNIBooleanArray;
	add: #JNIBooleanArrayG;
	add: #JNIByteArray;
	add: #JNIByteArrayG;
	add: #JNICharArray;
	add: #JNICharArrayG;
	add: #JNIClass;
	add: #JNIClassG;
	add: #JNIDoubleArray;
	add: #JNIDoubleArrayG;
	add: #JNIEnv;
	add: #JNIError;
	add: #JNIFieldID;
	add: #JNIFloatArray;
	add: #JNIFloatArrayG;
	add: #JNIGlobalRef;
	add: #JNIIntArray;
	add: #JNIIntArrayG;
	add: #JNIInvokeInterface;
	add: #JNILibrary;
	add: #JNILibraryForBEAJRockit7;
	add: #JNILibraryForBEAJRockit8;
	add: #JNILibraryForIBMJDK;
	add: #JNILibraryForIBMJRE;
	add: #JNILibraryForSunJ2SDK;
	add: #JNILibraryForSunJDK;
	add: #JNILibraryForSunJRE;
	add: #JNILibrarySunStyle;
	add: #JNILocalRef;
	add: #JNILongArray;
	add: #JNILongArrayG;
	add: #JNIMethodID;
	add: #JNINativeInterface;
	add: #JNINativeMethod;
	add: #JNIObject;
	add: #JNIObjectArray;
	add: #JNIObjectArrayG;
	add: #JNIObjectG;
	add: #JNIReference;
	add: #JNIShortArray;
	add: #JNIShortArrayG;
	add: #JNIString;
	add: #JNIStringG;
	add: #JNIThrowable;
	add: #JNIThrowableG;
	add: #JNIValue;
	add: #JNIValueArray;
	add: #JNIVTableClient;
	add: #JNIVTableStructure;
	add: #JNIWeakGlobalRef;
	yourself.

package globalNames
	add: #JNIConstants;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'CU Binary Arrays';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Registry\Dolphin Registry Access';
	yourself).

package!

"Class Definitions"!

Object subclass: #DeadJNIEnv
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DWORDField subclass: #FunctionPointerField
	instanceVariableNames: 'signature'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalAddress variableByteSubclass: #JNIFieldID
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalAddress variableByteSubclass: #JNIMethodID
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalAddress variableByteSubclass: #JNIReference
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: 'JNIConstants'
	classInstanceVariableNames: ''!
ExternalAddress variableByteSubclass: #JNIWeakGlobalRef
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNIReference variableByteSubclass: #JNIGlobalRef
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNIReference variableByteSubclass: #JNILocalRef
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNIGlobalRef variableByteSubclass: #JNIObjectG
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNIObjectG variableByteSubclass: #JNIArrayG
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNIObjectG variableByteSubclass: #JNIClassG
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNIObjectG variableByteSubclass: #JNIStringG
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNIObjectG variableByteSubclass: #JNIThrowableG
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNIArrayG variableByteSubclass: #JNIBooleanArrayG
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNIArrayG variableByteSubclass: #JNIByteArrayG
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNIArrayG variableByteSubclass: #JNICharArrayG
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNIArrayG variableByteSubclass: #JNIDoubleArrayG
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNIArrayG variableByteSubclass: #JNIFloatArrayG
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNIArrayG variableByteSubclass: #JNIIntArrayG
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNIArrayG variableByteSubclass: #JNILongArrayG
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNIArrayG variableByteSubclass: #JNIObjectArrayG
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNIArrayG variableByteSubclass: #JNIShortArrayG
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNILocalRef variableByteSubclass: #JNIObject
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNIObject variableByteSubclass: #JNIArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNIObject variableByteSubclass: #JNIClass
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNIObject variableByteSubclass: #JNIString
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNIObject variableByteSubclass: #JNIThrowable
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNIArray variableByteSubclass: #JNIBooleanArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNIArray variableByteSubclass: #JNIByteArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNIArray variableByteSubclass: #JNICharArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNIArray variableByteSubclass: #JNIDoubleArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNIArray variableByteSubclass: #JNIFloatArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNIArray variableByteSubclass: #JNIIntArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNIArray variableByteSubclass: #JNILongArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNIArray variableByteSubclass: #JNIObjectArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNIArray variableByteSubclass: #JNIShortArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Error subclass: #JNIError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalLibrary subclass: #JNILibrary
	instanceVariableNames: ''
	classVariableNames: 'EnableDevelopmentOptions ErrorStrings'
	poolDictionaries: 'CRTConstants JNIConstants'
	classInstanceVariableNames: ''!
JNILibrary subclass: #JNILibrarySunStyle
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNILibrarySunStyle subclass: #JNILibraryForBEAJRockit7
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNILibrarySunStyle subclass: #JNILibraryForBEAJRockit8
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNILibrarySunStyle subclass: #JNILibraryForIBMJDK
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNILibrarySunStyle subclass: #JNILibraryForIBMJRE
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNILibrarySunStyle subclass: #JNILibraryForSunJDK
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNILibrarySunStyle subclass: #JNILibraryForSunJRE
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNILibraryForSunJDK subclass: #JNILibraryForSunJ2SDK
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalStructure subclass: #JavaVMInitArgs
	instanceVariableNames: 'optionData optionRef'
	classVariableNames: ''
	poolDictionaries: 'JNIConstants'
	classInstanceVariableNames: ''!
ExternalStructure subclass: #JavaVMOption
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalStructure subclass: #JDK1_1InitArgs
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: 'JNIConstants'
	classInstanceVariableNames: ''!
ExternalStructure subclass: #JNINativeMethod
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalStructure subclass: #JNIValue
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalStructure subclass: #JNIVTableStructure
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: 'JNIConstants'
	classInstanceVariableNames: ''!
ExternalArray subclass: #JNIValueArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'zero'!
JNIVTableStructure subclass: #JNIInvokeInterface
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNIVTableStructure subclass: #JNINativeInterface
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LPVOID subclass: #JNIVTableClient
	instanceVariableNames: ''
	classVariableNames: 'Fixes'
	poolDictionaries: 'JNIConstants'
	classInstanceVariableNames: ''!
JNIVTableClient subclass: #JavaVM
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNIVTableClient subclass: #JNIEnv
	instanceVariableNames: 'javaVM'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

Smalltalk at: #JNIConstants put: (PoolConstantsDictionary named: #JNIConstants)!
JNIConstants at: 'JNI_ABORT' put: 16r2!
JNIConstants at: 'JNI_COMMIT' put: 16r1!
JNIConstants at: 'JNI_EDETACHED' put: -16r2!
JNIConstants at: 'JNI_EEXIST' put: -16r5!
JNIConstants at: 'JNI_EINVAL' put: -16r6!
JNIConstants at: 'JNI_ERR' put: -16r1!
JNIConstants at: 'JNI_EVERSION' put: -16r3!
JNIConstants at: 'JNI_NOMEM' put: -16r4!
JNIConstants at: 'JNI_OK' put: 16r0!
JNIConstants at: 'JNI_VERSION_1_1' put: 16r10001!
JNIConstants at: 'JNI_VERSION_1_2' put: 16r10002!
JNIConstants at: 'JNI_VERSION_1_3' put: 16r10003!
JNIConstants at: 'JNI_VERSION_1_4' put: 16r10004!
JNIConstants shrink!

"Classes"!

DeadJNIEnv guid: (GUID fromString: '{32DDF98B-708D-4A5B-8212-A0074D2700A8}')!
DeadJNIEnv comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

These things implement the "release resource" bit of the protocol understood by JNIEnv, they can be useful for keeping shutdown/cleanup code clean, since they can avoid the necessity for a load of #isNil checks.'!
!DeadJNIEnv categoriesForClass!Unclassified! !
!DeadJNIEnv methodsFor!

checkForException
	"answer whether there is a Java exception waiting to be handled"

	^ false.!

DeleteGlobalRef_obj: argument1
	"ignore"!

DeleteGlobalRef_obj: argument1 onException: a1Block
	"ignore"
!

DeleteLocalRef_obj: argument1
	"ignore"
!

DeleteLocalRef_obj: argument1 onException: a1Block
	"ignore"
!

DeleteWeakGlobalRef_obj: argument1
	"ignore"
!

DeleteWeakGlobalRef_obj: argument1 onException: a1Block
	"ignore"
!

doesNotUnderstand: aMessage
	"sent by the VM if we do not otherwise understand aMessage"

	^ (JNIEnv canUnderstand: aMessage selector)
		ifTrue: [JNIError signal: 'JNIEnv has been closed' with: -1]
		ifFalse: [super doesNotUnderstand: aMessage].!

ExceptionCheck
	"ignore"
!

ExceptionClear
	"ignore"
!

ExceptionDescribe
	"ignore"
!

exceptionDo: a1Block
	"ignore"
!

ExceptionOccurred
	"ignore"

	^ nil.!

ifExceptionDo: a1Block
	"ignore"
!

isDead

	^ true.!

IsSameObject_obj: argument1 obj: argument2
	"treat as always false"

	^ 0.!

IsSameObject_obj: argument1 obj: argument2 onException: a1Block
	"treat as always false"

	^ 0.
!

javaVM
	"answer the receiver's owning JavaVM instance"

	^ nil.
!

ReleaseBooleanArrayElements_array: argument1 elems: argument2 mode: argument3
	"ignore"
!

ReleaseBooleanArrayElements_array: argument1 elems: argument2 mode: argument3 onException: a1Block
	"ignore"
!

ReleaseByteArrayElements_array: argument1 elems: argument2 mode: argument3
	"ignore"
!

ReleaseByteArrayElements_array: argument1 elems: argument2 mode: argument3 onException: a1Block
	"ignore"
!

ReleaseCharArrayElements_array: argument1 elems: argument2 mode: argument3
	"ignore"
!

ReleaseCharArrayElements_array: argument1 elems: argument2 mode: argument3 onException: a1Block
	"ignore"
!

ReleaseDoubleArrayElements_array: argument1 elems: argument2 mode: argument3
	"ignore"
!

ReleaseDoubleArrayElements_array: argument1 elems: argument2 mode: argument3 onException: a1Block
	"ignore"
!

ReleaseFloatArrayElements_array: argument1 elems: argument2 mode: argument3
	"ignore"
!

ReleaseFloatArrayElements_array: argument1 elems: argument2 mode: argument3 onException: a1Block
	"ignore"
!

ReleaseIntArrayElements_array: argument1 elems: argument2 mode: argument3
	"ignore"
!

ReleaseIntArrayElements_array: argument1 elems: argument2 mode: argument3 onException: a1Block
	"ignore"
!

ReleaseLongArrayElements_array: argument1 elems: argument2 mode: argument3
	"ignore"
!

ReleaseLongArrayElements_array: argument1 elems: argument2 mode: argument3 onException: a1Block
	"ignore"
!

ReleasePrimitiveArrayCritical_array: argument1 carray: argument2 mode: argument3
	"ignore"
!

ReleasePrimitiveArrayCritical_array: argument1 carray: argument2 mode: argument3 onException: a1Block
	"ignore"
!

ReleaseShortArrayElements_array: argument1 elems: argument2 mode: argument3
	"ignore"
!

ReleaseShortArrayElements_array: argument1 elems: argument2 mode: argument3 onException: a1Block
	"ignore"
!

ReleaseStringChars_str: argument1 chars: argument2
	"ignore"
!

ReleaseStringChars_str: argument1 chars: argument2 onException: a1Block
	"ignore"
!

ReleaseStringCritical_str: argument1 cstring: argument2
	"ignore"
!

ReleaseStringCritical_str: argument1 cstring: argument2 onException: a1Block
	"ignore"
!

ReleaseStringUTFChars_str: argument1 chars: argument2
	"ignore"
!

ReleaseStringUTFChars_str: argument1 chars: argument2 onException: a1Block
	"ignore"
!

respondsTo: aSelector

	^ (super respondsTo: aSelector) or: [JNIEnv canUnderstand: aSelector].!

UnregisterNatives_class: argument1
	"ignore"
!

UnregisterNatives_class: argument1 onException: a1Block
	"ignore"
! !
!DeadJNIEnv categoriesFor: #checkForException!exceptions!public! !
!DeadJNIEnv categoriesFor: #DeleteGlobalRef_obj:!private!raw JNI functions! !
!DeadJNIEnv categoriesFor: #DeleteGlobalRef_obj:onException:!checked JNI functions!public! !
!DeadJNIEnv categoriesFor: #DeleteLocalRef_obj:!private!raw JNI functions! !
!DeadJNIEnv categoriesFor: #DeleteLocalRef_obj:onException:!checked JNI functions!public! !
!DeadJNIEnv categoriesFor: #DeleteWeakGlobalRef_obj:!private!raw JNI functions! !
!DeadJNIEnv categoriesFor: #DeleteWeakGlobalRef_obj:onException:!checked JNI functions!public! !
!DeadJNIEnv categoriesFor: #doesNotUnderstand:!exceptions!public! !
!DeadJNIEnv categoriesFor: #ExceptionCheck!exceptions!private!raw JNI functions! !
!DeadJNIEnv categoriesFor: #ExceptionClear!exceptions!private!raw JNI functions! !
!DeadJNIEnv categoriesFor: #ExceptionDescribe!exceptions!private!raw JNI functions! !
!DeadJNIEnv categoriesFor: #exceptionDo:!exceptions!public! !
!DeadJNIEnv categoriesFor: #ExceptionOccurred!exceptions!private!raw JNI functions! !
!DeadJNIEnv categoriesFor: #ifExceptionDo:!exceptions!public! !
!DeadJNIEnv categoriesFor: #isDead!public!testing! !
!DeadJNIEnv categoriesFor: #IsSameObject_obj:obj:!private!raw JNI functions! !
!DeadJNIEnv categoriesFor: #IsSameObject_obj:obj:onException:!checked JNI functions!public! !
!DeadJNIEnv categoriesFor: #javaVM!accessing!public! !
!DeadJNIEnv categoriesFor: #ReleaseBooleanArrayElements_array:elems:mode:!private!raw JNI functions! !
!DeadJNIEnv categoriesFor: #ReleaseBooleanArrayElements_array:elems:mode:onException:!checked JNI functions!public! !
!DeadJNIEnv categoriesFor: #ReleaseByteArrayElements_array:elems:mode:!private!raw JNI functions! !
!DeadJNIEnv categoriesFor: #ReleaseByteArrayElements_array:elems:mode:onException:!checked JNI functions!public! !
!DeadJNIEnv categoriesFor: #ReleaseCharArrayElements_array:elems:mode:!private!raw JNI functions! !
!DeadJNIEnv categoriesFor: #ReleaseCharArrayElements_array:elems:mode:onException:!checked JNI functions!public! !
!DeadJNIEnv categoriesFor: #ReleaseDoubleArrayElements_array:elems:mode:!private!raw JNI functions! !
!DeadJNIEnv categoriesFor: #ReleaseDoubleArrayElements_array:elems:mode:onException:!checked JNI functions!public! !
!DeadJNIEnv categoriesFor: #ReleaseFloatArrayElements_array:elems:mode:!private!raw JNI functions! !
!DeadJNIEnv categoriesFor: #ReleaseFloatArrayElements_array:elems:mode:onException:!checked JNI functions!public! !
!DeadJNIEnv categoriesFor: #ReleaseIntArrayElements_array:elems:mode:!private!raw JNI functions! !
!DeadJNIEnv categoriesFor: #ReleaseIntArrayElements_array:elems:mode:onException:!checked JNI functions!public! !
!DeadJNIEnv categoriesFor: #ReleaseLongArrayElements_array:elems:mode:!private!raw JNI functions! !
!DeadJNIEnv categoriesFor: #ReleaseLongArrayElements_array:elems:mode:onException:!checked JNI functions!public! !
!DeadJNIEnv categoriesFor: #ReleasePrimitiveArrayCritical_array:carray:mode:!private!raw JNI functions! !
!DeadJNIEnv categoriesFor: #ReleasePrimitiveArrayCritical_array:carray:mode:onException:!checked JNI functions!public! !
!DeadJNIEnv categoriesFor: #ReleaseShortArrayElements_array:elems:mode:!private!raw JNI functions! !
!DeadJNIEnv categoriesFor: #ReleaseShortArrayElements_array:elems:mode:onException:!checked JNI functions!public! !
!DeadJNIEnv categoriesFor: #ReleaseStringChars_str:chars:!private!raw JNI functions! !
!DeadJNIEnv categoriesFor: #ReleaseStringChars_str:chars:onException:!checked JNI functions!public! !
!DeadJNIEnv categoriesFor: #ReleaseStringCritical_str:cstring:!private!raw JNI functions! !
!DeadJNIEnv categoriesFor: #ReleaseStringCritical_str:cstring:onException:!checked JNI functions!public! !
!DeadJNIEnv categoriesFor: #ReleaseStringUTFChars_str:chars:!private!raw JNI functions! !
!DeadJNIEnv categoriesFor: #ReleaseStringUTFChars_str:chars:onException:!checked JNI functions!public! !
!DeadJNIEnv categoriesFor: #respondsTo:!public!testing! !
!DeadJNIEnv categoriesFor: #UnregisterNatives_class:!private!raw JNI functions! !
!DeadJNIEnv categoriesFor: #UnregisterNatives_class:onException:!checked JNI functions!public! !

FunctionPointerField guid: (GUID fromString: '{7EC2AE37-2797-482C-A2C7-1307C23E587F}')!
FunctionPointerField comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

To be honest, I don''t think there''s any need for these anymore (if, indeed, there ever was), but I haven''t got around to removing them...

Sorry!!'!
!FunctionPointerField categoriesForClass!Unclassified! !
!FunctionPointerField methodsFor!

initialize
	"private -- arrange to be readonly"

	super initialize.
	self beReadOnly.!

signature
	"private -- answer the receiver's function's signature as an ExternalDescriptor"

	^ signature.
!

signature: anExternalDescriptor
	"private -- set the receiver's function's signature to anExternalDescriptor"

	signature := anExternalDescriptor.
! !
!FunctionPointerField categoriesFor: #initialize!initializing!private! !
!FunctionPointerField categoriesFor: #signature!accessing!private! !
!FunctionPointerField categoriesFor: #signature:!accessing!private! !

!FunctionPointerField class methodsFor!

signature: anExternalDescriptor
	"answer a new instance of the receiver which is used for dereferencing
	function pointers with signature anExternalDescriptor"

	^ (self new)
		signature: anExternalDescriptor;
		yourself.! !
!FunctionPointerField class categoriesFor: #signature:!instance creation!public! !

JNIFieldID guid: (GUID fromString: '{A27D1207-6005-40D6-8BEB-109A719F8783}')!
JNIFieldID isIndirection: true!
JNIFieldID comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Equivalent to the JNI opaque pointer type, jfieldID.
'!
!JNIFieldID categoriesForClass!Unclassified! !
JNIMethodID guid: (GUID fromString: '{7B0BC86A-58E1-42ED-AA75-ED138F896518}')!
JNIMethodID isIndirection: true!
JNIMethodID comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Equivalent to the JNI opaque pointer type, jmethodID.
'!
!JNIMethodID categoriesForClass!Unclassified! !
JNIReference guid: (GUID fromString: '{0174440E-7B3A-4711-8A95-546B5927731C}')!
JNIReference isIndirection: true!
JNIReference comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

This class doesn''t actually do very much, except point out the commonality between JNILocalRef and JNIGlobalRef'!
!JNIReference categoriesForClass!Unclassified! !
!JNIReference methodsFor!

equals: aJNIReference jniEnv: aJNIEnv
	"use aJNIEnv to answer the result of calling the equals() Java method with aJNIObject as its parameter.
	This is provided mainly as an illustration of the kinds of things you have to do to use raw JNI"

	| class mid args byte |

	aJNIReference isNull ifTrue: [^ self isNull].

	"note that this implementation doesn't even make use of the few convenience
	features defined in this package"

	[
		"get the class"
		class := aJNIEnv GetObjectClass_obj: self.

		"find the method ID"
		mid := aJNIEnv
				GetMethodID_class: class
				name: 'equals'
				sig: '(Ljava/lang/Object;)Z'.
		mid isNull ifTrue: [aJNIEnv ExceptionClear. ^ nil].

		"create the argument array"
		args := (JNIValueArray new: 1)
				objectAt: 1 put: aJNIReference;
				yourself.

		"call the method"
		byte := aJNIEnv
				CallBooleanMethodA_obj: self
				methodID: mid
				args: args.
		(aJNIEnv ExceptionCheck = 0) ifFalse: [aJNIEnv ExceptionClear. ^ nil].

		"and decode the result"
		^ byte = 1.
	]
	ensure:
	[
		class isNil ifFalse: [aJNIEnv DeleteLocalRef_obj: class].
	].
!

getGlobalRef: aJNIEnv onException: a1Block
	"create a new JNIGlobalRef of the appropriate sub-class corresponding to our own,
	note that the existing ref is *not* cleared"

	| new |

	new := aJNIEnv
			NewGlobalRef_obj: self
			onException: a1Block.

	new becomeA: self globalRefClass.

	^ new.!

getLocalRef: aJNIEnv onException: a1Block
	"create a new JNILocalRef of the appropriate sub-class corresponding to our own,
	note that the existing ref is *not* cleared"

	| new |

	new := aJNIEnv
			NewLocalRef_obj: self
			onException: a1Block.

	new becomeA: self localRefClass.

	^ new.!

getWeakGlobalRef: aJNIEnv onException: a1Block
	"create a new JNIWeakGlobalRef pointing to the same Java object as we do,
	note that the existing ref is *not* cleared"

	^ aJNIEnv
		NewWeakGlobalRef_obj: self
		onException: a1Block.!

globalRefClass
	"answer the corresponding JNI global reference class"

	self subclassResponsibility.
!

isGlobalRef
	"answer whether the receiver represents a JNI global reference"

	self subclassResponsibility.
!

isLocalRef
	"answer whether the receiver represents a JNI local reference"

	self subclassResponsibility.
!

localRefClass
	"answer the corresponding JNI local reference class"

	self subclassResponsibility.!

releaseRef: aJNIEnv
	"release the JNI global reference we represent"

	self subclassResponsibility.
!

releaseRef: aJNIEnv onException: a1Block
	"release the JNI global reference we represent"

	self subclassResponsibility.
!

toString: aJNIEnv
	"use aJNIEnv to convert the receiver to a Smalltalk String.
	This is provided mainly as an illustration of the kinds of things you have to
	do to use raw JNI"

	| class mid jstring length bytes byteArray |

	self isNull ifTrue: [^ nil].

	"note that this implementation doesn't even make use of the few convenience
	features defined in this package"

	[
		"get the class"
		class := aJNIEnv GetObjectClass_obj: self.

		"find the method ID"
		mid := aJNIEnv
				GetMethodID_class: class
				name: 'toString'
				sig: '()Ljava/lang/String;'.
		mid isNull ifTrue: [aJNIEnv ExceptionClear. ^ nil].

		"call the method (it has no args which makes it simpler)"
		jstring := aJNIEnv
				CallObjectMethodA_obj: self
				methodID: mid
				args: nil.
		(aJNIEnv ExceptionCheck = 0) ifFalse: [aJNIEnv ExceptionClear. ^ nil].

		"get the length of the string in bytes (we'll assume this can't fail)"
		length := aJNIEnv GetStringUTFLength_str: jstring.
		length = 0 ifTrue: [^ ''].

		"fish out the bytes from the jstring; note that we must remember to give them back later"
		bytes := aJNIEnv GetStringUTFChars_str: jstring isCopy: nil.
		(aJNIEnv ExceptionCheck = 0) ifFalse: [aJNIEnv ExceptionClear. ^ nil].

		"and turn them into a Smalltalk string -- easy 'eh ?"
		byteArray := BYTEArray fromAddress: bytes length: length.
		^ String fromJavaQuasiUTF8EncodedByteArray: byteArray.
	]
	ensure:
	[
		bytes isNil ifFalse: [aJNIEnv ReleaseStringUTFChars_str: jstring chars: bytes].
		jstring isNil ifFalse: [aJNIEnv DeleteLocalRef_obj: jstring].
		class isNil ifFalse: [aJNIEnv DeleteLocalRef_obj: class].
	].! !
!JNIReference categoriesFor: #equals:jniEnv:!comparing!examples!public! !
!JNIReference categoriesFor: #getGlobalRef:onException:!operations!public! !
!JNIReference categoriesFor: #getLocalRef:onException:!operations!public! !
!JNIReference categoriesFor: #getWeakGlobalRef:onException:!operations!public! !
!JNIReference categoriesFor: #globalRefClass!constants!public! !
!JNIReference categoriesFor: #isGlobalRef!public!testing! !
!JNIReference categoriesFor: #isLocalRef!public!testing! !
!JNIReference categoriesFor: #localRefClass!constants!public! !
!JNIReference categoriesFor: #releaseRef:!operations!public! !
!JNIReference categoriesFor: #releaseRef:onException:!operations!public! !
!JNIReference categoriesFor: #toString:!converting!examples!public! !

!JNIReference class methodsFor!

refsInUse
	"answer a count of how many non-null instances still exist
		JNIGlobalRef refsInUse. 
		JNILocalRef refsInUse. 
	"

	| count |

	count := 0.
	self allSubinstances do: [:each | each isNull ifFalse: [count := count + 1]].

	^ count.! !
!JNIReference class categoriesFor: #refsInUse!analysing!public! !

JNIWeakGlobalRef guid: (GUID fromString: '{F726418B-E235-4F80-AE4D-AE290BB60018}')!
JNIWeakGlobalRef isIndirection: true!
JNIWeakGlobalRef comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

These correspond to JNI''s weak global references to Java objects.

Just about the only way you can safely use these things is by converting them temporarily to a strong ref.  You *can* test whether they are still alive, and even whether they are the same as some existing Java object, but that''s all.  (Which is the main reason why this is not a sub-class of JNIGlobalRef, or JNIReference -- our instances aren''t substitutable for theirs.)'!
!JNIWeakGlobalRef categoriesForClass!Unclassified! !
!JNIWeakGlobalRef methodsFor!

getGlobalRef: aJNIEnv onException: a1Block
	"create a new JNIObjectG pointing to the same underlying Java object as we
	(may) do.  Answers a NULL pointer if our object has been GCed,
	Note that we can't 'downcast' to the best sub-class of JNIObjectG, so we
	don't"

	^ aJNIEnv
		NewGlobalRef_obj: self
		onException: a1Block.!

getLocalRef: aJNIEnv onException: a1Block
	"create a new JNIObject pointing to the same underlying Java object as we
	(may) do.  Answers a NULL pointer if our object has been GCed,
	Note that we can't 'downcast' to the best sub-class of JNIObject, so we
	don't"

	^ aJNIEnv
		NewLocalRef_obj: self
		onException: a1Block.!

getWeakGlobalRef: aJNIEnv onException: a1Block
	"create a new JNIWeakGlobalRef pointing to the same Java object as we do"

	^ aJNIEnv
		NewWeakGlobalRef_obj: self
		onException: a1Block.!

releaseRef: aJNIEnv
	"release the JNI global reference we represent"

	aJNIEnv DeleteWeakGlobalRef_obj: self.
!

releaseRef: aJNIEnv onException: a1Block
	"release the JNI global reference we represent"

	aJNIEnv
		DeleteWeakGlobalRef_obj: self
		onException: a1Block.
! !
!JNIWeakGlobalRef categoriesFor: #getGlobalRef:onException:!operations!public! !
!JNIWeakGlobalRef categoriesFor: #getLocalRef:onException:!operations!public! !
!JNIWeakGlobalRef categoriesFor: #getWeakGlobalRef:onException:!operations!public! !
!JNIWeakGlobalRef categoriesFor: #releaseRef:!operations!public! !
!JNIWeakGlobalRef categoriesFor: #releaseRef:onException:!operations!public! !

JNIGlobalRef guid: (GUID fromString: '{C18CDA77-56F6-4D0A-BE37-C7EB4A96CC91}')!
JNIGlobalRef isIndirection: true!
JNIGlobalRef comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

These are references to Java objects via JNI.  The subtypes correspond to the ''C++'' opaque reference types in the JNI documentation.

Note that they are intended to represent objects allocated in the global pool.  There is a similar heirarchy of objects corresponding to local refs.

Note that the only way these are created is via the raw NewGlobalRef() method which only knows that it returns a global, not which subclass of JNIObjectG is should be in. The helper method JNILocalRef>>#getGlobalRef:onException: will answer an object of the appropriate class.

The helper method #getLocalRef:onException: will reverse the conversion.'!
!JNIGlobalRef categoriesForClass!Unclassified! !
!JNIGlobalRef methodsFor!

globalRefClass
	"answer the corresponding JNI global reference class"

	self class.
!

isGlobalRef
	"answer whether the receiver represents a JNI global reference"

	^ true.!

isLocalRef
	"answer whether the receiver represents a JNI local reference"

	^ false.!

localRefClass
	"answer the corresponding JNI local reference class"

	self subclassResponsibility.!

releaseRef: aJNIEnv
	"release the JNI global reference we represent"

	aJNIEnv DeleteGlobalRef_obj: self.
!

releaseRef: aJNIEnv onException: a1Block
	"release the JNI global reference we represent"

	aJNIEnv
		DeleteGlobalRef_obj: self
		onException: a1Block.
! !
!JNIGlobalRef categoriesFor: #globalRefClass!constants!public! !
!JNIGlobalRef categoriesFor: #isGlobalRef!public!testing! !
!JNIGlobalRef categoriesFor: #isLocalRef!public!testing! !
!JNIGlobalRef categoriesFor: #localRefClass!constants!public! !
!JNIGlobalRef categoriesFor: #releaseRef:!operations!public! !
!JNIGlobalRef categoriesFor: #releaseRef:onException:!operations!public! !

JNILocalRef guid: (GUID fromString: '{5F5650D4-02EE-4E65-8F1E-DBF7432264F4}')!
JNILocalRef isIndirection: true!
JNILocalRef comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

These are references to Java objects via JNI.  The subtypes correspond to the ''C++'' opaque reference types in the JNI documentation.

Note that they are intended to represent objects allocated in the local ref pool.  There is a similar heirarchy of objects corresponding to global refs, but note that the raw NewGlobalRef() method only knows that it returns a global, not which subclass of JNIObjectG is should be in.  The helper method #getGlobalRef:onException: will do the appropriate conversion.'!
!JNILocalRef categoriesForClass!Unclassified! !
!JNILocalRef methodsFor!

globalRefClass
	"answer the corresponding JNI global reference class"

	self subclassResponsibility.
!

isGlobalRef
	"answer whether the receiver represents a JNI global reference"

	^ false.!

isLocalRef
	"answer whether the receiver represents a JNI local reference"

	^ true.!

localRefClass
	"answer the corresponding JNI local reference class"

	self class.!

releaseRef: aJNIEnv
	"release the JNI global reference we represent"

	aJNIEnv DeleteLocalRef_obj: self.
!

releaseRef: aJNIEnv onException: a1Block
	"release the JNI global reference we represent"

	aJNIEnv
		DeleteLocalRef_obj: self
		onException: a1Block.
! !
!JNILocalRef categoriesFor: #globalRefClass!constants!public! !
!JNILocalRef categoriesFor: #isGlobalRef!public!testing! !
!JNILocalRef categoriesFor: #isLocalRef!public!testing! !
!JNILocalRef categoriesFor: #localRefClass!constants!public! !
!JNILocalRef categoriesFor: #releaseRef:!operations!public! !
!JNILocalRef categoriesFor: #releaseRef:onException:!operations!public! !

JNIObjectG guid: (GUID fromString: '{66674670-ADC1-42EC-AED0-EA2ACBC94AC3}')!
JNIObjectG isIndirection: true!
JNIObjectG comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Equivalent to the JNI opaque pointer type, jobject, except that we "know" that we are a global ref.
'!
!JNIObjectG categoriesForClass!Unclassified! !
!JNIObjectG methodsFor!

localRefClass
	"answer the corresponding JNI local reference class"

	^JNIObject.! !
!JNIObjectG categoriesFor: #localRefClass!constants!public! !

JNIArrayG guid: (GUID fromString: '{48F74B7F-5DD9-4AA5-A3C8-77EE60DD090A}')!
JNIArrayG isIndirection: true!
JNIArrayG comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Equivalent to the JNI opaque pointer type, jarray, except that we "know" that we are a global ref.
'!
!JNIArrayG categoriesForClass!Unclassified! !
!JNIArrayG methodsFor!

localRefClass
	"answer the corresponding JNI local reference class"

	^JNIArray.! !
!JNIArrayG categoriesFor: #localRefClass!constants!public! !

JNIClassG guid: (GUID fromString: '{BDE832F4-056A-4E97-A7E2-E48649D879B1}')!
JNIClassG isIndirection: true!
JNIClassG comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Equivalent to the JNI opaque pointer type, jclass, except that we "know" that we are a global ref.
'!
!JNIClassG categoriesForClass!Unclassified! !
!JNIClassG methodsFor!

localRefClass
	"answer the corresponding JNI local reference class"

	^JNIClass.! !
!JNIClassG categoriesFor: #localRefClass!constants!public! !

JNIStringG guid: (GUID fromString: '{FF234E1A-B4D5-434C-B33E-5B34E28808BC}')!
JNIStringG isIndirection: true!
JNIStringG comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Equivalent to the JNI opaque pointer type, jstring, except that we "know" that we are a global ref.
'!
!JNIStringG categoriesForClass!Unclassified! !
!JNIStringG methodsFor!

localRefClass
	"answer the corresponding JNI local reference class"

	^JNIString.! !
!JNIStringG categoriesFor: #localRefClass!constants!public! !

JNIThrowableG guid: (GUID fromString: '{CC192878-5B20-4B3B-85A4-DB65DBB5D218}')!
JNIThrowableG isIndirection: true!
JNIThrowableG comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Equivalent to the JNI opaque pointer type, jthrowable, except that we "know" that we are a global ref.
'!
!JNIThrowableG categoriesForClass!Unclassified! !
!JNIThrowableG methodsFor!

localRefClass
	"answer the corresponding JNI local reference class"

	^JNIThrowable.! !
!JNIThrowableG categoriesFor: #localRefClass!constants!public! !

JNIBooleanArrayG guid: (GUID fromString: '{C3CE9B3D-E5B4-4429-899F-D3191CCC11A0}')!
JNIBooleanArrayG isIndirection: true!
JNIBooleanArrayG comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Equivalent to the JNI opaque pointer type, jbooleanArray, except that we "know" that we are a global ref.
'!
!JNIBooleanArrayG categoriesForClass!Unclassified! !
!JNIBooleanArrayG methodsFor!

localRefClass
	"answer the corresponding JNI local reference class"

	^JNIBooleanArray.! !
!JNIBooleanArrayG categoriesFor: #localRefClass!constants!public! !

JNIByteArrayG guid: (GUID fromString: '{F58681E1-C44B-44F3-ACC1-223CA64DD778}')!
JNIByteArrayG isIndirection: true!
JNIByteArrayG comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Equivalent to the JNI opaque pointer type, jbyteArray, except that we "know" that we are a global ref.'!
!JNIByteArrayG categoriesForClass!Unclassified! !
!JNIByteArrayG methodsFor!

localRefClass
	"answer the corresponding JNI local reference class"

	^JNIByteArray.! !
!JNIByteArrayG categoriesFor: #localRefClass!constants!public! !

JNICharArrayG guid: (GUID fromString: '{E89C6492-838F-49F5-A4DE-0A783BF0A84A}')!
JNICharArrayG isIndirection: true!
JNICharArrayG comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Equivalent to the JNI opaque pointer type, jcharArray, except that we "know" that we are a global ref.
'!
!JNICharArrayG categoriesForClass!Unclassified! !
!JNICharArrayG methodsFor!

localRefClass
	"answer the corresponding JNI local reference class"

	^JNICharArray.! !
!JNICharArrayG categoriesFor: #localRefClass!constants!public! !

JNIDoubleArrayG guid: (GUID fromString: '{15FF2B04-7F8A-489B-8260-DD85DCFB63DE}')!
JNIDoubleArrayG isIndirection: true!
JNIDoubleArrayG comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Equivalent to the JNI opaque pointer type, jdoubleArray, except that we "know" that we are a global ref.
'!
!JNIDoubleArrayG categoriesForClass!Unclassified! !
!JNIDoubleArrayG methodsFor!

localRefClass
	"answer the corresponding JNI local reference class"

	^JNIDoubleArray.! !
!JNIDoubleArrayG categoriesFor: #localRefClass!constants!public! !

JNIFloatArrayG guid: (GUID fromString: '{5A17184C-900C-418F-B939-3CE04272048C}')!
JNIFloatArrayG isIndirection: true!
JNIFloatArrayG comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Equivalent to the JNI opaque pointer type, jfloatArray, except that we "know" that we are a global ref.
'!
!JNIFloatArrayG categoriesForClass!Unclassified! !
!JNIFloatArrayG methodsFor!

localRefClass
	"answer the corresponding JNI local reference class"

	^JNIFloatArray.! !
!JNIFloatArrayG categoriesFor: #localRefClass!constants!public! !

JNIIntArrayG guid: (GUID fromString: '{9655AEEB-D354-47F7-90B0-8463D19199CA}')!
JNIIntArrayG isIndirection: true!
JNIIntArrayG comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Equivalent to the JNI opaque pointer type, jintArray, except that we "know" that we are a global ref.
'!
!JNIIntArrayG categoriesForClass!Unclassified! !
!JNIIntArrayG methodsFor!

localRefClass
	"answer the corresponding JNI local reference class"

	^JNIIntArray.! !
!JNIIntArrayG categoriesFor: #localRefClass!constants!public! !

JNILongArrayG guid: (GUID fromString: '{9C34561F-BA52-42D6-9985-D0F0A1081624}')!
JNILongArrayG isIndirection: true!
JNILongArrayG comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Equivalent to the JNI opaque pointer type, jlongArray, except that we "know" that we are a global ref.
'!
!JNILongArrayG categoriesForClass!Unclassified! !
!JNILongArrayG methodsFor!

localRefClass
	"answer the corresponding JNI local reference class"

	^JNILongArray.! !
!JNILongArrayG categoriesFor: #localRefClass!constants!public! !

JNIObjectArrayG guid: (GUID fromString: '{7E711AD0-29C2-461D-9BDE-50CB93488BED}')!
JNIObjectArrayG isIndirection: true!
JNIObjectArrayG comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Equivalent to the JNI opaque pointer type, jobjectArray, except that we "know" that we are a global ref.
'!
!JNIObjectArrayG categoriesForClass!Unclassified! !
!JNIObjectArrayG methodsFor!

localRefClass
	"answer the corresponding JNI local reference class"

	^JNIObjectArray.! !
!JNIObjectArrayG categoriesFor: #localRefClass!constants!public! !

JNIShortArrayG guid: (GUID fromString: '{7A3DA5E6-5D28-49DC-8B49-DEC80140C5B3}')!
JNIShortArrayG isIndirection: true!
JNIShortArrayG comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Equivalent to the JNI opaque pointer type, jshortArray, except that we "know" that we are a global ref.
'!
!JNIShortArrayG categoriesForClass!Unclassified! !
!JNIShortArrayG methodsFor!

localRefClass
	"answer the corresponding JNI local reference class"

	^JNIShortArray.! !
!JNIShortArrayG categoriesFor: #localRefClass!constants!public! !

JNIObject guid: (GUID fromString: '{AE980EEF-8B7E-4D76-B573-48E3C9B81A93}')!
JNIObject isIndirection: true!
JNIObject comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Equivalent to the JNI opaque pointer type, jobject.
'!
!JNIObject categoriesForClass!Unclassified! !
!JNIObject methodsFor!

globalRefClass
	"answer the corresponding JNI global reference class"

	^JNIObjectG.! !
!JNIObject categoriesFor: #globalRefClass!constants!public! !

JNIArray guid: (GUID fromString: '{FE80A68D-1389-4B4B-97A1-CA887259C3AF}')!
JNIArray isIndirection: true!
JNIArray comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Equivalent to the JNI opaque pointer type, jarray.
'!
!JNIArray categoriesForClass!Unclassified! !
!JNIArray methodsFor!

globalRefClass
	"answer the corresponding JNI global reference class"

	^JNIArrayG.! !
!JNIArray categoriesFor: #globalRefClass!constants!public! !

JNIClass guid: (GUID fromString: '{17066126-564A-4BEB-9F9E-9D47778339D7}')!
JNIClass isIndirection: true!
JNIClass comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Equivalent to the JNI opaque pointer type, jclass.
'!
!JNIClass categoriesForClass!Unclassified! !
!JNIClass methodsFor!

globalRefClass
	"answer the corresponding JNI global reference class"

	^JNIClassG.! !
!JNIClass categoriesFor: #globalRefClass!constants!public! !

JNIString guid: (GUID fromString: '{ADC57521-3612-4C9D-948D-8E1EBEDB8B54}')!
JNIString isIndirection: true!
JNIString comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Equivalent to the JNI opaque pointer type, jstring.
'!
!JNIString categoriesForClass!Unclassified! !
!JNIString methodsFor!

globalRefClass
	"answer the corresponding JNI global reference class"

	^JNIStringG.! !
!JNIString categoriesFor: #globalRefClass!constants!public! !

JNIThrowable guid: (GUID fromString: '{8A2F74AE-F8DA-4623-83B7-C0F81D90785A}')!
JNIThrowable isIndirection: true!
JNIThrowable comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Equivalent to the JNI opaque pointer type, jthrowable.
'!
!JNIThrowable categoriesForClass!Unclassified! !
!JNIThrowable methodsFor!

globalRefClass
	"answer the corresponding JNI global reference class"

	^JNIThrowableG.! !
!JNIThrowable categoriesFor: #globalRefClass!constants!public! !

JNIBooleanArray guid: (GUID fromString: '{DF30ECD2-5C47-4B60-ADB5-71B6ED31738B}')!
JNIBooleanArray isIndirection: true!
JNIBooleanArray comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Equivalent to the JNI opaque pointer type, jbooleanArray.
'!
!JNIBooleanArray categoriesForClass!Unclassified! !
!JNIBooleanArray methodsFor!

globalRefClass
	"answer the corresponding JNI global reference class"

	^JNIBooleanArrayG.! !
!JNIBooleanArray categoriesFor: #globalRefClass!constants!public! !

JNIByteArray guid: (GUID fromString: '{D650AB1F-E874-4F96-8192-067E94CEB0AE}')!
JNIByteArray isIndirection: true!
JNIByteArray comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Equivalent to the JNI opaque pointer type, jbyteArray.
'!
!JNIByteArray categoriesForClass!Unclassified! !
!JNIByteArray methodsFor!

globalRefClass
	"answer the corresponding JNI global reference class"

	^JNIByteArrayG.! !
!JNIByteArray categoriesFor: #globalRefClass!constants!public! !

JNICharArray guid: (GUID fromString: '{F6900EF1-BDC7-4DBE-BA08-B9A1BCBADE2C}')!
JNICharArray isIndirection: true!
JNICharArray comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Equivalent to the JNI opaque pointer type, jcharArray.
'!
!JNICharArray categoriesForClass!Unclassified! !
!JNICharArray methodsFor!

globalRefClass
	"answer the corresponding JNI global reference class"

	^JNICharArrayG.! !
!JNICharArray categoriesFor: #globalRefClass!constants!public! !

JNIDoubleArray guid: (GUID fromString: '{BAAB562D-0CEA-4E86-98C5-3687E64E083C}')!
JNIDoubleArray isIndirection: true!
JNIDoubleArray comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Equivalent to the JNI opaque pointer type, jdoubleArray.
'!
!JNIDoubleArray categoriesForClass!Unclassified! !
!JNIDoubleArray methodsFor!

globalRefClass
	"answer the corresponding JNI global reference class"

	^JNIDoubleArrayG.! !
!JNIDoubleArray categoriesFor: #globalRefClass!constants!public! !

JNIFloatArray guid: (GUID fromString: '{FC3D08EA-8D5A-418D-9D96-107817234C52}')!
JNIFloatArray isIndirection: true!
JNIFloatArray comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Equivalent to the JNI opaque pointer type, jfloatArray.
'!
!JNIFloatArray categoriesForClass!Unclassified! !
!JNIFloatArray methodsFor!

globalRefClass
	"answer the corresponding JNI global reference class"

	^JNIFloatArrayG.! !
!JNIFloatArray categoriesFor: #globalRefClass!constants!public! !

JNIIntArray guid: (GUID fromString: '{7DE4BDA5-7703-4F84-B312-400A4157C540}')!
JNIIntArray isIndirection: true!
JNIIntArray comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Equivalent to the JNI opaque pointer type, jintArray.
'!
!JNIIntArray categoriesForClass!Unclassified! !
!JNIIntArray methodsFor!

globalRefClass
	"answer the corresponding JNI global reference class"

	^JNIIntArrayG.! !
!JNIIntArray categoriesFor: #globalRefClass!constants!public! !

JNILongArray guid: (GUID fromString: '{1DFEC2E4-3A42-45E0-A03D-91661F59E658}')!
JNILongArray isIndirection: true!
JNILongArray comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Equivalent to the JNI opaque pointer type, jlongArray.
'!
!JNILongArray categoriesForClass!Unclassified! !
!JNILongArray methodsFor!

globalRefClass
	"answer the corresponding JNI global reference class"

	^JNILongArrayG.! !
!JNILongArray categoriesFor: #globalRefClass!constants!public! !

JNIObjectArray guid: (GUID fromString: '{48DA6168-3D2F-4C59-A4A5-084B0BBEF172}')!
JNIObjectArray isIndirection: true!
JNIObjectArray comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Equivalent to the JNI opaque pointer type, jobjectArray.
'!
!JNIObjectArray categoriesForClass!Unclassified! !
!JNIObjectArray methodsFor!

globalRefClass
	"answer the corresponding JNI global reference class"

	^JNIObjectArrayG.! !
!JNIObjectArray categoriesFor: #globalRefClass!constants!public! !

JNIShortArray guid: (GUID fromString: '{2C489582-442A-463E-8DBA-6933FA1F79EB}')!
JNIShortArray isIndirection: true!
JNIShortArray comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Equivalent to the JNI opaque pointer type, jshortArray.
'!
!JNIShortArray categoriesForClass!Unclassified! !
!JNIShortArray methodsFor!

globalRefClass
	"answer the corresponding JNI global reference class"

	^JNIShortArrayG.! !
!JNIShortArray categoriesFor: #globalRefClass!constants!public! !

JNIError guid: (GUID fromString: '{CFDB1C46-CE8F-4E5C-B6EA-31D32F8326BE}')!
JNIError comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Special error subclass used for errors detected in JNI.  Note, these are *not* used to wrap Java exceptions.'!
!JNIError categoriesForClass!Unclassified! !
!JNIError methodsFor!

_descriptionArguments
	"answer the arguments to be substituted into the receiver's Win32 description format String"

	^ Array 
		with: self tag displayString
		with: self messageText.
!

_descriptionFormat
	"Answer the Win32 format String to be used to format the description for the receiver."

	^ 'JNI Error: %1 (%2)'.! !
!JNIError categoriesFor: #_descriptionArguments!displaying!public! !
!JNIError categoriesFor: #_descriptionFormat!displaying!public! !

JNILibrary guid: (GUID fromString: '{9AABF318-0884-4B79-B2F5-4B41FFE735A8}')!
JNILibrary comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Abstract class which provides the common implementation of the JNI entry-points (but not the ones accessed via the ''environment'' (array of pointers to functions).  This is abstract because any vendor''s implementation is apt to have different methods of locating the JVM.'!
!JNILibrary categoriesForClass!Unclassified! !
!JNILibrary methodsFor!

checkReturnCode: aNumber
	"private -- check the return code from a JNI call, and -- if it is not OK -- throw a corresponding JNIError"

	aNumber = JNI_OK ifFalse: [self signalError: aNumber].!

createFirstJNIEnv: aJavaVMInitArgs
	"create a new JavaVM and JNIEnv pair using the given JavaVMInitArgs;
	answers the JNIEnv instance (from which the JavaVM instance can be reached)"

	| javaVM jniEnv |

	javaVM := JavaVM new.
	jniEnv := JNIEnv newWithJavaVM: javaVM.
	self createJavaVM: javaVM jniEnv: jniEnv options: aJavaVMInitArgs.

	^ jniEnv.!

createJavaVM: aJavaVM jniEnv: aJNIEnv options: aJVMInitArgs
	"private -- attempt to create a new JavaVM and JNIEnv with the given options.
	The results are written back to the LPVOID objects aJavaVM and aJNIEnv, throws an
	exception if if fails"

	| return |

	return := self JNI_CreateJavaVM: aJavaVM penv: aJNIEnv args: aJVMInitArgs.
	self checkReturnCode: return.
!

errorClass
	"private -- answer the class of Exception to use to signal JNI errors"

	^ JNIError.!

getDefaultJavaVMInitArgs
	"answer an object which the referred to library considers appropriately populated
	to use by default for creating a JVM.
	Note that this has been unecessary since JNI 1.2 (== JDK/j2SDK 1.3)"

	| args |

	args := JDK1_1InitArgs new.
	^ (self JNI_GetDefaultJavaVMInitArgs: args) = JNI_OK
		ifTrue: [args]
		ifFalse: [nil].
!

getJavaVMs
	"answer anArray holding pointers to all the JavaVMs that this JNI library knows about"

	| vms nvms  |

	vms := PointerArray length: 0 elementClass: JavaVM.
	nvms := self getJavaVMs: vms.
	vms := PointerArray length: nvms elementClass: JavaVM.
	nvms := self getJavaVMs: vms.

	self assert: [nvms = vms size].

	^ vms asArray.
!

getJavaVMs: aPointerArray
	"private -- fill in a PointerArray (which is assumed to have been created to hold pointers to
	JavaVMs by asking the JNI library for a list of all the JVMs it has created.  Up to
		aPointerArray size
	elements will be filled in, and the answer is how many it actually knows about (which might be
	more than aPointerArray can hold)"

	| return nvms  |

	nvms := DWORD new.
	return := self
			JNI_GetCreatedJavaVMs: aPointerArray
			bufLen: aPointerArray size
			nVMs: nvms.
	self checkReturnCode: return.

	^ nvms value.
!

JNI_CreateJavaVM: aJavaVM penv: aJNIEnv args: aJVMInitArgs
	"
		jint JNI_CreateJavaVM(JavaVM **pvm, JNIEnv **penv, void *args)
	"

	<stdcall: sdword JNI_CreateJavaVM JavaVM** JNIEnv** void*>
	^ self invalidCall.
!

JNI_GetCreatedJavaVMs: vmBuf bufLen: bufLen nVMs: nVMs
	"
		jint JNI_GetCreatedJavaVMs(JavaVM **vmBuf, jsize bufLen, jsize *nVMs)
	"

	<stdcall: sdword JNI_GetCreatedJavaVMs JavaVM** sdword sdword*>
	^ self invalidCall.
!

JNI_GetDefaultJavaVMInitArgs: vm_args
	"
		jint JNI_GetDefaultJavaVMInitArgs(void *vm_args)
	"

	<stdcall: sdword JNI_GetDefaultJavaVMInitArgs void*>
	^ self invalidCall.
!

name
	"answer a name for this JNI library instance"

	"a workable default"
	^ self fileName.!

signalError: aNumber
	"private -- throw an exception corresponding to the given JNI error code"

	self errorClass
		signal: (self class lookupErrorCode: aNumber)
		with: aNumber.! !
!JNILibrary categoriesFor: #checkReturnCode:!helpers!private! !
!JNILibrary categoriesFor: #createFirstJNIEnv:!operations!public! !
!JNILibrary categoriesFor: #createJavaVM:jniEnv:options:!operations!private! !
!JNILibrary categoriesFor: #errorClass!constants!exceptions!private! !
!JNILibrary categoriesFor: #getDefaultJavaVMInitArgs!accessing!public! !
!JNILibrary categoriesFor: #getJavaVMs!accessing!public! !
!JNILibrary categoriesFor: #getJavaVMs:!accessing!private! !
!JNILibrary categoriesFor: #JNI_CreateJavaVM:penv:args:!private!raw JNI functions! !
!JNILibrary categoriesFor: #JNI_GetCreatedJavaVMs:bufLen:nVMs:!private!raw JNI functions! !
!JNILibrary categoriesFor: #JNI_GetDefaultJavaVMInitArgs:!private!raw JNI functions! !
!JNILibrary categoriesFor: #name!accessing!private! !
!JNILibrary categoriesFor: #signalError:!exceptions!private! !

!JNILibrary class methodsFor!

currentVersion
	"answer the the 'current version' of the JVM"

	self subclassResponsibility.!

defaultServerType
	"private -- answer the default 'type' of the JVM as a String.  The answer is
	vendor/version-dependent, but for example, a Sun (1.4) runtime
	will be 'client' or  'server' "

	self subclassResponsibility.!

enableDevelopmentOptions
	"answer whether JNIPort development options should be enabled"

	^ EnableDevelopmentOptions ifNil: [false].
!

enableDevelopmentOptions: aBool
	"set whether JNIPort development options should be enabled

		self enableDevelopmentOptions: false.
		self enableDevelopmentOptions: true.
	"

	EnableDevelopmentOptions := aBool.!

fileName
	"answer the host system file name of the external library which the 
	receiver represents.  Note that this is the 'current version' of the JVM
	library, other versions can be loaded via #version:"

	^ self filenameForCurrentVersion.!

filenameForCurrentVersion
	"answer the host system file name of the 'current version' of the JVM"

	^ self filenameForVersion: self currentVersion.!

filenameForVersion: aVersionString
	"answer the host system file name of the given version of the JVM"

	^ self filenameForVersion: aVersionString jvmType: self defaultServerType.!

filenameForVersion: aVersionString jvmType: aTypeString
	"answer the host system file name of the given version and type of the JVM.
	The valid type[s] is vendor/version-dependent, but for example, a Sun runtime
	will be 'client' or  'server' -- at least as of J2SDK 1.4."

	self subclassResponsibility.
!

initialize
	"private -- class initialisation.

		self initialize.
	"

	"note that I use the literal constants for initialising this table, since
	I'm not certain that the pool constants dictionary will be set correctly
	when this is called"

	ErrorStrings := (IdentityDictionary new)
				at: "JNI_OK"			( 0) put: 'success';
				at: "JNI_ERR"		(-1) put: 'unknown error';
				at: "JNI_EDETACHED"	(-2) put: 'thread detached from the VM';
				at: "JNI_EVERSION"	(-3) put: 'JNI version error';
				at: "JNI_ENOMEM"		(-4) put: 'not enough memory';
				at: "JNI_EEXIST"		(-5) put: 'VM already created';
				at: "JNI_EINVAL"		(-6) put: 'invalid arguments';
				shrink.
!

isAbstract
	"answer whether the receiver is an abstract class, this is used only
	in order to help construct the list of available subclasses in the JVM
	settings UI"

	^ self == ##(self).!

lookupErrorCode: aNumber
	"answer an error string corresponding to the given JNI error code"

	^ ErrorStrings at: aNumber ifAbsent: ['undefined JNI error code'].!

rebuildPoolConstants
	"private -- rebuild the JNI pool constants dictionary.

		self rebuildPoolConstants.
	"

	(Smalltalk at: #JNIConstants ifAbsentPut: [PoolConstantsDictionary new])

		"JNI versions (*not* JDK versions!!)"
		at: 'JNI_VERSION_1_1'	put: 16r00010001;
		at: 'JNI_VERSION_1_2'	put: 16r00010002;
		at: 'JNI_VERSION_1_3'	put: 16r00010003;		"this was never officially defined"
		at: 'JNI_VERSION_1_4'	put: 16r00010004;

		"possible return values from JNI functions"
		at: 'JNI_OK' 			put: 0;
		at: 'JNI_ERR'			put: -1;
		at: 'JNI_EDETACHED'	put: -2;
		at: 'JNI_EVERSION'		put: -3;
		at: 'JNI_NOMEM'		put: -4;
		at: 'JNI_EEXIST'		put: -5;
		at: 'JNI_EINVAL'		put: -6;

		"used in Release*ArrayElements()"
		at: 'JNI_COMMIT'		put: 1;
		at: 'JNI_ABORT'		put: 2;

		shrink.
!

version: aVersionString
	"answer an instance of the receiver which references the specified version
	of the JDK java libary"

	^self open: (self filenameForVersion: aVersionString).!

version: aVersionString jvmType: aTypeString
	"answer an instance of the receiver which references the specified version
	of the JDK java libary, and which uses the specified type of JVM"

	^self open: (self filenameForVersion: aVersionString jvmType: aTypeString).! !
!JNILibrary class categoriesFor: #currentVersion!locating runtime!public! !
!JNILibrary class categoriesFor: #defaultServerType!locating runtime!private! !
!JNILibrary class categoriesFor: #enableDevelopmentOptions!accessing!public! !
!JNILibrary class categoriesFor: #enableDevelopmentOptions:!accessing!public! !
!JNILibrary class categoriesFor: #fileName!constants!public! !
!JNILibrary class categoriesFor: #filenameForCurrentVersion!locating runtime!public! !
!JNILibrary class categoriesFor: #filenameForVersion:!locating runtime!public! !
!JNILibrary class categoriesFor: #filenameForVersion:jvmType:!locating runtime!public! !
!JNILibrary class categoriesFor: #initialize!initializing!private! !
!JNILibrary class categoriesFor: #isAbstract!public!testing! !
!JNILibrary class categoriesFor: #lookupErrorCode:!accessing!public! !
!JNILibrary class categoriesFor: #rebuildPoolConstants!initializing!private! !
!JNILibrary class categoriesFor: #version:!instance creation!public! !
!JNILibrary class categoriesFor: #version:jvmType:!instance creation!public! !

JNILibrarySunStyle guid: (GUID fromString: '{84D1D3FA-8647-44D6-97C2-EB5CF44BD4F0}')!
JNILibrarySunStyle comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Abstract class that adds more machinery to JNILibrary for finding the runtime assuming a vendor that has followed (more or less) Sun''s lead in how to make a complete pig''s ear of finding the runtime in the Windows registry. '!
!JNILibrarySunStyle categoriesForClass!Unclassified! !
!JNILibrarySunStyle class methodsFor!

binName
	"private -- answer the relative name of the 'bin' directory where the JVM-type specific stuff lives"

	^ 'bin'.!

currentVersion
	"answer the the 'current version' of the JVM"

	^ self registryEntryRoot valueAt: self registryCurrentVersionName.!

filenameForVersion: aVersionString jvmType: aTypeString
	"answer the host system file name of the given version and type of the JVM.
	The valid type[s] is vendor/version-dependent, but for example, a Sun runtime
	will be 'client' or  'server' -- at least as of J2SDK 1.4."

	| home bin subdir |

	"typical case is <java_runtime_home>/bin/<server_type>/jvm.dll"

	home := self javaRuntimeHomeForVersion: aVersionString.
	bin := File composePath: home subPath: self binName.
	subdir := File composePath: bin subPath: aTypeString.

	^  File composePath: subdir subPath: self jvmDLLName.
!

isAbstract
	"answer whether the receiver is an abstract class, this is used only
	in order to help construct the list of available subclasses in the JVM
	settings UI"

	^ self == ##(self).!

javaHome
	"answer the the JavaHome of the 'current version' of the JVM"

	^ self javaHomeForVersion: self currentVersion.
!

javaHomeForVersion: aVersionString
	"answer the JavaHome (installation root) of the given version of the JDK"

	^ (self registryEntryForVersion: aVersionString) valueAt: (self registryJavaHomeName).!

javaRuntimeHome
	"answer where the runtime 'current version' of the JVM lives"

	^ self javaRuntimeHomeForVersion: self currentVersion.!

javaRuntimeHomeForVersion: aVersionString
	"answer the name of the directory where the Java runtime is located"

	"normally the same as the java home itself"
	^ self javaHomeForVersion: aVersionString.!

jvmDLLName
	"private -- answer the relative name of the JVM.DLL'"

	^ 'jvm.dll'.!

registryCurrentVersionName
	"private -- answer the name of the registry entry where the current version is specified"

	^ 'CurrentVersion'.!

registryEntryForCurrentVersion
	"private -- answer the registry data for the current version of the JVM"

	^ self registryEntryForVersion: self currentVersion.!

registryEntryForVersion: aVersionString
	"private -- answer the registry data for the given version of the JVM"

	^ self registryEntryRoot at: aVersionString.!

registryEntryRoot
	"private -- answer a RegKey representing the root of our registry entry"

	^ (RegKeyAbstract localMachineRoot: #read) at: (self registryRootName).
!

registryJavaHomeName
	"private -- answer the name of the registry entry where the installation root is specified"

	^ 'JavaHome'.!

registryRootName
	"private -- answer the name of the root of the JRE registry entry"

	self subclassResponsibility.! !
!JNILibrarySunStyle class categoriesFor: #binName!constants!private! !
!JNILibrarySunStyle class categoriesFor: #currentVersion!locating runtime!public! !
!JNILibrarySunStyle class categoriesFor: #filenameForVersion:jvmType:!locating runtime!public! !
!JNILibrarySunStyle class categoriesFor: #isAbstract!public!testing! !
!JNILibrarySunStyle class categoriesFor: #javaHome!locating runtime!public! !
!JNILibrarySunStyle class categoriesFor: #javaHomeForVersion:!locating runtime!public!registry! !
!JNILibrarySunStyle class categoriesFor: #javaRuntimeHome!locating runtime!public! !
!JNILibrarySunStyle class categoriesFor: #javaRuntimeHomeForVersion:!accessing!public!registry! !
!JNILibrarySunStyle class categoriesFor: #jvmDLLName!constants!private! !
!JNILibrarySunStyle class categoriesFor: #registryCurrentVersionName!constants!private!registry! !
!JNILibrarySunStyle class categoriesFor: #registryEntryForCurrentVersion!private!registry! !
!JNILibrarySunStyle class categoriesFor: #registryEntryForVersion:!private!registry! !
!JNILibrarySunStyle class categoriesFor: #registryEntryRoot!private!registry! !
!JNILibrarySunStyle class categoriesFor: #registryJavaHomeName!constants!private!registry! !
!JNILibrarySunStyle class categoriesFor: #registryRootName!constants!private!registry! !

JNILibraryForBEAJRockit7 guid: (GUID fromString: '{67918C0E-6FC4-4849-9E6F-1ABF5EAA64FC}')!
JNILibraryForBEAJRockit7 comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Version of the library that knows how BEA''s "JRockit" JVM stores its location in the registry.

JRockit is available at:

	http://www.bea.com/products/weblogic/jrockit/index.shtml

Note that this release of JRockit requires the ''CU Java JRockit Patch'' package to be loaded *and* "installed" or very little will work (see that package''s comment for instructions and details).
'!
!JNILibraryForBEAJRockit7 categoriesForClass!Unclassified! !
!JNILibraryForBEAJRockit7 class methodsFor!

defaultServerType
	"private -- answer the default 'type' of the JVM as a String.  The answer is
	vendor/version-dependent, but for example, a Sun (1.4) runtime
	will be 'client' or  'server' "

	^ 'jrockit'.!

registryRootName
	"private -- answer the name of the root of the JDK registry entry"

	^ 'SOFTWARE\BEA Systems\JRockit\7.0\JRE'.
! !
!JNILibraryForBEAJRockit7 class categoriesFor: #defaultServerType!locating runtime!private! !
!JNILibraryForBEAJRockit7 class categoriesFor: #registryRootName!constants!private!registry! !

JNILibraryForBEAJRockit8 guid: (GUID fromString: '{71D8907D-7FB6-446C-991E-E8002FF9E69F}')!
JNILibraryForBEAJRockit8 comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Version of the library that knows how BEA''s "JRockit" JVM stores its location in the registry.

JRockit is available at:

	http://www.bea.com/products/weblogic/jrockit/index.shtml

Note that, judging from the beta, this release of JRockit requires the ''CU Java JRockit Patch'' package to be loaded *and* "installed" or very little will work (see that package''s comment for instructions and details).
'!
!JNILibraryForBEAJRockit8 categoriesForClass!Unclassified! !
!JNILibraryForBEAJRockit8 class methodsFor!

defaultServerType
	"private -- answer the default 'type' of the JVM as a String.  The answer is
	vendor/version-dependent, but for example, a Sun (1.4) runtime
	will be 'client' or  'server' "

	^ 'jrockit'.!

registryRootName
	"private -- answer the name of the root of the JDK registry entry"

	^ 'SOFTWARE\BEA Systems\JRockit\8.0\JRE'.
! !
!JNILibraryForBEAJRockit8 class categoriesFor: #defaultServerType!locating runtime!private! !
!JNILibraryForBEAJRockit8 class categoriesFor: #registryRootName!constants!private!registry! !

JNILibraryForIBMJDK guid: (GUID fromString: '{53E0D630-8D41-42FC-88B2-5A2135A5489D}')!
JNILibraryForIBMJDK comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Version of the library that knows how IBM''s version of the JDK stores its location in the registry.

The IBM JDK is available at:

	http://www7b.boulder.ibm.com/wsdd/wspvtdevkit-info.html'!
!JNILibraryForIBMJDK categoriesForClass!Unclassified! !
!JNILibraryForIBMJDK class methodsFor!

defaultServerType
	"private -- answer the default 'type' of the JVM as a String.  The answer is
	vendor/version-dependent, but for example, a Sun (1.4) runtime
	will be 'client' or  'server' "

	^ 'classic'.!

javaRuntimeHomeForVersion: aVersionString
	"answer the name of the directory where the Java runtime is located"

	^ File
		composePath: (self javaHomeForVersion: aVersionString)
		subPath: 'jre'.
!

registryRootName
	"private -- answer the name of the root of the JDK registry entry"

	^ 'SOFTWARE\IBM\Java Development Kit'.! !
!JNILibraryForIBMJDK class categoriesFor: #defaultServerType!locating runtime!private! !
!JNILibraryForIBMJDK class categoriesFor: #javaRuntimeHomeForVersion:!locating runtime!public! !
!JNILibraryForIBMJDK class categoriesFor: #registryRootName!constants!private!registry! !

JNILibraryForIBMJRE guid: (GUID fromString: '{CAC6296E-4D87-41E2-AC77-740BFE6DD2C9}')!
JNILibraryForIBMJRE comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Note: I''ve never actually installed just the IBM JRE, so this implementation is just a guess.  Call it a placeholder if you like.

Version of the library that "knows" how IBM''s version of the JRE stores its location in the registry.

The IBM JRE is available at:

	http://www7b.boulder.ibm.com/wsdd/wspvtjre-info.html'!
!JNILibraryForIBMJRE categoriesForClass!Unclassified! !
!JNILibraryForIBMJRE class methodsFor!

defaultServerType
	"private -- answer the default 'type' of the JVM as a String.  The answer is
	vendor/version-dependent, but for example, a Sun (1.4) runtime
	will be 'client' or  'server' "

	^ 'classic'.!

registryRootName
	"private -- answer the name of the root of the JDK registry entry"

	^ 'SOFTWARE\IBM\Java Runtime Environment'.! !
!JNILibraryForIBMJRE class categoriesFor: #defaultServerType!locating runtime!private! !
!JNILibraryForIBMJRE class categoriesFor: #registryRootName!constants!private!registry! !

JNILibraryForSunJDK guid: (GUID fromString: '{86606AE8-CEEB-4222-9381-9D8268006365}')!
JNILibraryForSunJDK comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Version of the library that knows how Sun''s version of the JDK stores its location in the registry.

The JDK is available at:

	http://java.sun.com/j2se/'!
!JNILibraryForSunJDK categoriesForClass!Unclassified! !
!JNILibraryForSunJDK class methodsFor!

defaultServerType
	"answer the default 'type' of the JVM as a String.  The answer is
	vendor/version-dependent, but for example, a Sun (1.4) runtime
	will be 'client' or  'server' "

	"Sun provides no way to get at this, so we we just guess"
	^ 'client'.!

javaRuntimeHomeForVersion: aVersionString
	"answer the name of the directory where the Java runtime is located"

	^ File
		composePath: (self javaHomeForVersion: aVersionString)
		subPath: 'jre'.
!

registryRootName
	"private -- answer the name of the root of the JDK registry entry"

	^ 'SOFTWARE\JavaSoft\Java Development Kit'.! !
!JNILibraryForSunJDK class categoriesFor: #defaultServerType!locating runtime!public! !
!JNILibraryForSunJDK class categoriesFor: #javaRuntimeHomeForVersion:!locating runtime!public! !
!JNILibraryForSunJDK class categoriesFor: #registryRootName!constants!private!registry! !

JNILibraryForSunJRE guid: (GUID fromString: '{00FC544C-93D5-4FBA-89D7-B90A9C1BCD01}')!
JNILibraryForSunJRE comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Version of the library that knows how Sun''s version of the JRE stores its location in the registry.

The JRE is available at:

	http://java.sun.com/j2se/'!
!JNILibraryForSunJRE categoriesForClass!Unclassified! !
!JNILibraryForSunJRE class methodsFor!

defaultServerType
	"answer the default 'type' of the JVM as a String.  The answer is
	vendor/version-dependent, but for example, a Sun (1.4) runtime
	will be 'client' or  'server' "

	| dll dir |

	"messy!!"
	dll := self filenameForCurrentVersion.
	dir := File splitPathFrom: dll.
	dir := File removePathDelimiter: dir.
	^ File splitFilenameFrom: dir.
	!

filenameForVersion: aVersionString
	"answer the host system file name of the given version of the JVM"

	"overriden since Sun provide a direct record of what the default JVM is for each version"
	^ (self registryEntryForVersion: aVersionString) valueAt: self registryRuntimeLibraryName.!

registryRootName
	"private -- answer the name of the root of the JRE registry entry"

	^ 'SOFTWARE\JavaSoft\Java Runtime Environment'.!

registryRuntimeLibraryName
	"private -- answer the name of the registry entry where the runtime library is specified"

	^ 'RuntimeLib'.! !
!JNILibraryForSunJRE class categoriesFor: #defaultServerType!locating runtime!public! !
!JNILibraryForSunJRE class categoriesFor: #filenameForVersion:!locating runtime!public!registry! !
!JNILibraryForSunJRE class categoriesFor: #registryRootName!constants!private!registry! !
!JNILibraryForSunJRE class categoriesFor: #registryRuntimeLibraryName!constants!private!registry! !

JNILibraryForSunJ2SDK guid: (GUID fromString: '{04975E4A-1571-49A4-B825-0CFC6F4A3C1E}')!
JNILibraryForSunJ2SDK comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Since Sun have now -- at last -- reverted to calling the JDK "the JDK", I have followed suite.  This class is just a deprecated placeholder.'!
!JNILibraryForSunJ2SDK categoriesForClass!Unclassified! !
!JNILibraryForSunJ2SDK class methodsFor!

isAbstract

	"mark ourself as 'abstract' so as not to show up in the selection lists"
	^ true! !
!JNILibraryForSunJ2SDK class categoriesFor: #isAbstract!public!testing! !

JavaVMInitArgs guid: (GUID fromString: '{4B41D33E-B9B1-4DAA-8A30-0B4F7AAA6872}')!
JavaVMInitArgs comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Wrapper for JNI''s JavaVMInitArgs type.'!
!JavaVMInitArgs categoriesForClass!Unclassified! !
!JavaVMInitArgs methodsFor!

addAbortHook: anExternalCallback
	"add an option to set the abort() hook.  It should have the signature:
		stdcall void
	"

	"it appears that you have to take the address of a callback's #asParameter before
	storing a ref to it as an LPVOIDField in an external structure; this is unlike the case
	where it is passed to an external function as a lpvoid parameter, in which case #yourAddress
	should *not* be sent.  I find Dolphin's external interfacing *extremely* confusing!!"
	self addOption: 'abort' extraInfo: anExternalCallback asParameter yourAddress.
!

addClasspath: aString
	"add a class path option to the set we already have"

	self addProperty: 'java.class.path' value: aString.
!

addClassPath: aString
	"add a class path option to the set we already have"

	self addClasspath: aString.
!

addCompiler: aString
	"add a compiler option to the set we already have"

	self addProperty: 'java.compiler' value: aString.
!

addExitHook: anExternalCallback
	"add an option to set the exit() hook.  It should have the signature:
		stdcall void sdword
	"

	"it appears that you have to take the address of a callback's #asParameter before
	storing a ref to it as an LPVOIDField in an external structure; this is unlike the case
	where it is passed to an external function as a lpvoid parameter, in which case #yourAddress
	should *not* be sent.  I find Dolphin's external interfacing *extremely* confusing!!"
	self addOption: 'exit' extraInfo: anExternalCallback asParameter yourAddress.
!

addLibrarypath: aString
	"add a library path option to the set we already have"

	self addProperty: 'java.library.path' value: aString.
!

addLibraryPath: aString
	"add a library path option to the set we already have"

	self addLibrarypath: aString.
!

addOption: aString
	"add an option to the set we already have"

	self addOption: aString extraInfo: 0.
!

addOption: aString extraInfo: anObject
	"add an option to the set we already have"

	optionData at: aString put: anObject.
	self rebuildOptions.!

addOptions: aList
	"add more options to the set we already have"

	aList do: [:each | optionData at: each put: 0].
	self rebuildOptions.
!

addProperties: aDictionary
	"add more options to the set we already have"

	aDictionary keysAndValuesDo: [:key :value | optionData at: ('-D' , key , '=' , value) put: 0].
	self rebuildOptions.
!

addProperty: aNameString value: aValueString
	"add an 'property' option to the set we already have"

	self addOption: '-D' , aNameString , '=' , aValueString.
!

addVerbose: aString
	"add an verbosity option to the set we already have, the verbosity is
	a comma-separated list of strings.  The JNI spec defines: 'class', 'gc', and 'jni';
	other (JVM-specific) values should have names starting with an X"

	self addOption: ('-verbose:' , aString).
!

addVFPrintfHook: aCallback
	"add an option to set the vfprintf() hook..  It should have the signature:

		stdcall sdword void* char* void*

	the third argument is a pointer to a va_list (i.e. a block of dwords where each element
	contains one parameter to the varargs function).

	E.g: you could use:
		ExternalCallback
			block: [:fp :format :args | Transcript nextPutAll: (format vsprintfWith: args)]
			descriptor: (ExternalDescriptor fromString: 'stdcall: sdword void* char* void*').


	Note that the hook will be garbage-collected unless there is some other
	reference to it"

	self addOption: 'vfprintf' extraInfo: aCallback asParameter yourAddress.
!

ignoreUnrecognized
	"Answer the receiver's ignoreUnrecognized field as a Smalltalk object."

	^(bytes dwordAtOffset: 12) asBoolean.!

ignoreUnrecognized: anObject
	"Set the receiver's ignoreUnrecognized field to the value of anObject."

	bytes dwordAtOffset: 12 put: anObject asParameter.!

initialize
	"private -- establish a coherent initial state"

	super initialize.

	self version: JNI_VERSION_1_2.
	optionData := LookupTable new.
	optionRef := self options.	"we have to keep a reference outside the ExternalData"!

nOptions
	"Answer the receiver's nOptions field as a Smalltalk object."

	^(bytes sdwordAtOffset: 4).!

nOptions: anObject
	"Set the receiver's nOptions field to the value of anObject."

	bytes sdwordAtOffset: 4 put: anObject.!

options
	"Answer the receiver's options field as a Smalltalk object."

	^StructureArray fromAddress: (bytes sdwordAtOffset: 8) length: self nOptions elementClass: JavaVMOption.!

options: anObject
	"Set the receiver's options field to the value of anObject."

	bytes dwordAtOffset: 8 put: anObject yourAddress.!

rebuildOptions
	"private -- rebuild our array of JavaVMOptions to reflect the new contents
	of our real options"

	| i |

	optionRef := StructureArray length: optionData size elementClass: JavaVMOption.

	i := 1.
	optionData keysAndValuesDo: [:key :value | (optionRef at: i) optionString: key; extraInfo: value. i := i + 1].

	self nOptions: optionData size.
	self options: optionRef.
!

version
	"Answer the receiver's version field as a Smalltalk object."

	^(bytes sdwordAtOffset: 0).!

version: anObject
	"Set the receiver's version field to the value of anObject."

	bytes sdwordAtOffset: 0 put: anObject.! !
!JavaVMInitArgs categoriesFor: #addAbortHook:!adding!public! !
!JavaVMInitArgs categoriesFor: #addClasspath:!adding!public! !
!JavaVMInitArgs categoriesFor: #addClassPath:!adding!public! !
!JavaVMInitArgs categoriesFor: #addCompiler:!adding!public! !
!JavaVMInitArgs categoriesFor: #addExitHook:!adding!public! !
!JavaVMInitArgs categoriesFor: #addLibrarypath:!adding!public! !
!JavaVMInitArgs categoriesFor: #addLibraryPath:!adding!public! !
!JavaVMInitArgs categoriesFor: #addOption:!adding!public! !
!JavaVMInitArgs categoriesFor: #addOption:extraInfo:!adding!public! !
!JavaVMInitArgs categoriesFor: #addOptions:!adding!public! !
!JavaVMInitArgs categoriesFor: #addProperties:!adding!public! !
!JavaVMInitArgs categoriesFor: #addProperty:value:!adding!public! !
!JavaVMInitArgs categoriesFor: #addVerbose:!adding!public! !
!JavaVMInitArgs categoriesFor: #addVFPrintfHook:!adding!public! !
!JavaVMInitArgs categoriesFor: #ignoreUnrecognized!**compiled accessors**!public! !
!JavaVMInitArgs categoriesFor: #ignoreUnrecognized:!**compiled accessors**!public! !
!JavaVMInitArgs categoriesFor: #initialize!initializing!private! !
!JavaVMInitArgs categoriesFor: #nOptions!**compiled accessors**!public! !
!JavaVMInitArgs categoriesFor: #nOptions:!**compiled accessors**!public! !
!JavaVMInitArgs categoriesFor: #options!**compiled accessors**!public! !
!JavaVMInitArgs categoriesFor: #options:!**compiled accessors**!public! !
!JavaVMInitArgs categoriesFor: #rebuildOptions!helpers!private! !
!JavaVMInitArgs categoriesFor: #version!**compiled accessors**!public! !
!JavaVMInitArgs categoriesFor: #version:!**compiled accessors**!public! !

!JavaVMInitArgs class methodsFor!

defineFields
	"
	self compileDefinition.

	typedef struct JavaVMInitArgs {
		jint version;

		jint nOptions;
		JavaVMOption *options;
		jboolean ignoreUnrecognized;
	} JavaVMInitArgs;"

	self

		defineField: #version type: SDWORDField new;
		defineField: #nOptions type: SDWORDField new;
		defineField: #options type: (VariableStructureArrayPointerField type: JavaVMOption length: #nOptions);
		defineField: #ignoreUnrecognized type: BOOLField new;

		yourself.
! !
!JavaVMInitArgs class categoriesFor: #defineFields!constants!public! !

JavaVMOption guid: (GUID fromString: '{FD5AB26A-9390-4B3B-B8B8-3A4FEF9FF5FF}')!
JavaVMOption comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Wrapper for JNI''s JavaVMOptions type.
'!
!JavaVMOption categoriesForClass!Unclassified! !
!JavaVMOption methodsFor!

extraInfo
	"Answer the receiver's extraInfo field as a Smalltalk object."

	^(bytes dwordAtOffset: 4) asExternalAddress.!

extraInfo: anObject
	"Set the receiver's extraInfo field to the value of anObject."

	bytes dwordAtOffset: 4 put: anObject.!

optionString
	"Answer the receiver's optionString field as a Smalltalk object."

	^String fromAddress: (bytes sdwordAtOffset: 0).!

optionString: anObject
	"Set the receiver's optionString field to the value of anObject."

	bytes dwordAtOffset: 0 put: anObject yourAddress.! !
!JavaVMOption categoriesFor: #extraInfo!**compiled accessors**!public! !
!JavaVMOption categoriesFor: #extraInfo:!**compiled accessors**!public! !
!JavaVMOption categoriesFor: #optionString!**compiled accessors**!public! !
!JavaVMOption categoriesFor: #optionString:!**compiled accessors**!public! !

!JavaVMOption class methodsFor!

defineFields
	"
	self compileDefinition.

	typedef struct JavaVMOption {
		char *optionString;
		void *extraInfo;
	} JavaVMOption;"

	self
		defineField: #optionString type: (PointerField type: String);
		defineField: #extraInfo type: LPVOIDField new.!

optionString: aString

	^ (self new)
		optionString: aString;
		yourself.!

optionString: aString extraInfo: anExternalAddress

	^ (self optionString: aString)
		extraInfo: anExternalAddress;
		yourself.! !
!JavaVMOption class categoriesFor: #defineFields!constants!public! !
!JavaVMOption class categoriesFor: #optionString:!instance creation!public! !
!JavaVMOption class categoriesFor: #optionString:extraInfo:!instance creation!public! !

JDK1_1InitArgs guid: (GUID fromString: '{00000000-0000-0000-0000-000000000000}')!
JDK1_1InitArgs comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

This class only exists for backward compatability, the JNI corresponding JNI struct has been replaced by the (much more rational) JavaVMArgs'!
!JDK1_1InitArgs categoriesForClass!Unclassified! !
!JDK1_1InitArgs methodsFor!

initialize
	"private -- establish a default state"

	"this class is only useful with JNI 1.1 (it was rendered unecessary in 1.2), so
	we may as well set the version here"

	self version: JNI_VERSION_1_1.
! !
!JDK1_1InitArgs categoriesFor: #initialize!initializing!private! !

!JDK1_1InitArgs class methodsFor!

defineFields
	"
	self compileDefinition.

	typedef struct JDK1_1InitArgs {
		jint version;
		
		char **properties;
		jint checkSource;
		jint nativeStackSize;
		jint javaStackSize;
		jint minHeapSize;
		jint maxHeapSize;
		jint verifyMode;
		char *classpath;
		
		jint (JNICALL *vfprintf)(FILE *fp, const char *format, va_list args);
		void (JNICALL *exit)(jint code);
		void (JNICALL *abort)(void);
		
		jint enableClassGC;
		jint enableVerboseGC;
		jint disableAsyncGC;
		jint verbose;
		jboolean debugging;
		jint debugPort;
	} JDK1_1InitArgs;"

	self
		defineField: #version type: SDWORDField new;
		defineField: #properties type: (PointerArrayPointerField type: String);
		defineField: #checkSource type: SDWORDField new;
		defineField: #nativeStackSize type: SDWORDField new;
		defineField: #javaStackSize type: SDWORDField new;
		defineField: #minHeapSize type: SDWORDField new;
		defineField: #maxHeapSize type: SDWORDField new;
		defineField: #verifyMode type: SDWORDField new;
		defineField: #classpath type: (PointerField type: String);
		
		defineField: #vfprintf type: LPVOIDField new;
		defineField: #exit type: LPVOIDField new;
		defineField: #abort type: LPVOIDField new;
		
		defineField: #enableClassGC type: SDWORDField new;
		defineField: #enableVerboseGC type: SDWORDField new;
		defineField: #disableAsyncGC type: SDWORDField new;
		defineField: #verbose type: SDWORDField new;
		defineField: #debugging type: BOOLField new;
		defineField: #debugPort type: SDWORDField new;

		yourself.
! !
!JDK1_1InitArgs class categoriesFor: #defineFields!constants!public! !

JNINativeMethod guid: (GUID fromString: '{577BCC8E-B950-4909-9D52-93C94163E8C9}')!
JNINativeMethod comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Wrapper for JNI''s JavaNativeMethod type.
'!
!JNINativeMethod categoriesForClass!Unclassified! !
!JNINativeMethod methodsFor!

callback: anExternalCallback
	"set the receiver's callback field anExternalCallback"

	"as ever, we have to screw around to get an ExternalCallback into a void*"
	bytes dwordAtOffset: 8 put: anExternalCallback asParameter yourAddress.!

name
	"answer our String name"

	^ String fromAddress: (bytes sdwordAtOffset: 0).!

name: aString
	"set the receiver's name field to aString"

	bytes dwordAtOffset: 0 put: aString yourAddress.!

signature
	"answer our String signature"

	^ String fromAddress: (bytes sdwordAtOffset: 4).!

signature: aString
	"set the receiver's signature field aString"

	bytes dwordAtOffset: 4 put: aString yourAddress.! !
!JNINativeMethod categoriesFor: #callback:!accessing!public! !
!JNINativeMethod categoriesFor: #name!accessing!public! !
!JNINativeMethod categoriesFor: #name:!accessing!public! !
!JNINativeMethod categoriesFor: #signature!accessing!public! !
!JNINativeMethod categoriesFor: #signature:!accessing!public! !

!JNINativeMethod class methodsFor!

defineFields
	"
	self compileDefinition.

	typedef struct {
		char *name;
		char *signature;
		void *callback;
	} JNINativeMethod
;"

	self
		defineField: #name type: (PointerField type: String);
		defineField: #signature type: (PointerField type: String);
		defineField: #callback type: LPVOIDField writeOnly;
		yourself.! !
!JNINativeMethod class categoriesFor: #defineFields!constants!public! !

JNIValue guid: (GUID fromString: '{BC603FC8-D011-469E-8CF4-09B435B28001}')!
JNIValue comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Wrapper for JNI''s jvalue union type.

Typically there''s no need to use these directly, use the JNIValueArray structure instead for greater efficiency.
'!
!JNIValue categoriesForClass!Unclassified! !
!JNIValue methodsFor!

b: anInteger
	"alias supplied to allow the use of Sun's odd field names"

	^ self byte: anInteger.!

boolean
	"answer the receiver's boolean field as a Smalltalk Bool.
	Re-written from the auto-generated version, to answer a Bool
	form a Byte field"

	^ (bytes byteAtOffset: 0) == 1.!

boolean: aBool
	"Set the receiver's boolean field to the value of aBool encoded
	as 0 or 1"

	bytes byteAtOffset: 0 put: (aBool ifTrue: [1] ifFalse: [0]).!

byte: anInteger
	"set the receiver's byte field to anInteger"

	"#sbyteAtOffset:put: doesn't check, so we do it manually"
	(anInteger < -128 or: [anInteger > 127]) ifTrue: [self errorCantHold: anInteger].

	bytes sbyteAtOffset: 0 put: anInteger.!

c: aCharacterOrInteger
	"alias supplied to allow the use of Sun's odd field names"

	^ self char: aCharacterOrInteger.!

char
	"answer the receiver's char field as either a Smalltalk Character or an Integer
	depending on whether it'll fit as a 8-bit char.
	Re-written from the auto-generated version, to answer a Character (if possible)
	from a word field"

	| ch |

	ch := bytes wordAtOffset: 0.
	^ ch < 256
		ifTrue: [Character value: ch]
		ifFalse: [ch].!

char: aCharacterOrInteger
	"set the receiver's char field to the value of aCharOrInteger,
	Note that the integer value should fit in an unsigned 16bit word (i.e.
	be a valid Java char code)"

	bytes wordAtOffset: 0 put: aCharacterOrInteger.!

d: aFloat
	"alias supplied to allow the use of Sun's odd field names"

	^ self double: aFloat.!

double: anObject
	"Set the receiver's double field to the value of anObject."

	bytes doubleAtOffset: 0 put: anObject!

f: aFloat
	"alias supplied to allow the use of Sun's odd field names"

	^ self float: aFloat.!

float: anObject
	"Set the receiver's float field to the value of anObject."

	bytes floatAtOffset: 0 put: anObject!

i: anInteger
	"alias supplied to allow the use of Sun's odd field names"

	^ self int: anInteger.
!

int: anObject
	"Set the receiver's int field to the value of anObject."

	bytes sdwordAtOffset: 0 put: anObject!

j: anInteger
	"alias supplied to allow the use of Sun's odd field names"

	^ self long: anInteger.!

l: aJNIObjectOrNil
	"alias supplied to allow the use of Sun's odd field names"

	^ self object: aJNIObjectOrNil.
!

long: anObject
	"Set the receiver's long field to the value of anObject."

	bytes sqwordAtOffset: 0 put: anObject!

object: aJNIObjectOrNil
	"assign to the 'object' field of the receiver.
	(NB: although this isn't part of the 'CU Java Base' package, this formulation
	will work with JavaObject's too)"

	bytes dwordAtOffset: 0 put: aJNIObjectOrNil asParameter yourAddress.!

printOn: aStream
	"append a developer oriented representation of the receiver to aStream.
	Overridden to print out *something* (since all our fields are write-only so we don't try to read,
	say, Bools as JNIObjects)"

	aStream
		basicPrint: self;
		display: '(';
		display: self bytes;
		display: ')'.
!

s: anInteger
	"alias supplied to allow the use of Sun's odd field names"

	^ self short: anInteger.
!

short: anObject
	"Set the receiver's short field to the value of anObject."

	bytes swordAtOffset: 0 put: anObject!

z: aBool
	"alias supplied to allow the use of Sun's odd field names"

	^ self boolean: aBool.! !
!JNIValue categoriesFor: #b:!accessing!public! !
!JNIValue categoriesFor: #boolean!accessing!public! !
!JNIValue categoriesFor: #boolean:!accessing!public! !
!JNIValue categoriesFor: #byte:!accessing!public! !
!JNIValue categoriesFor: #c:!accessing!public! !
!JNIValue categoriesFor: #char!accessing!public! !
!JNIValue categoriesFor: #char:!accessing!public! !
!JNIValue categoriesFor: #d:!accessing!public! !
!JNIValue categoriesFor: #double:!**compiled accessors**!public! !
!JNIValue categoriesFor: #f:!accessing!public! !
!JNIValue categoriesFor: #float:!**compiled accessors**!public! !
!JNIValue categoriesFor: #i:!accessing!public! !
!JNIValue categoriesFor: #int:!**compiled accessors**!public! !
!JNIValue categoriesFor: #j:!accessing!public! !
!JNIValue categoriesFor: #l:!accessing!public! !
!JNIValue categoriesFor: #long:!**compiled accessors**!public! !
!JNIValue categoriesFor: #object:!accessing!public! !
!JNIValue categoriesFor: #printOn:!printing!public! !
!JNIValue categoriesFor: #s:!accessing!public! !
!JNIValue categoriesFor: #short:!**compiled accessors**!public! !
!JNIValue categoriesFor: #z:!accessing!public! !

!JNIValue class methodsFor!

boolean: aBool
	"answer a new instance wrapping aBool"

	^ (self new)
		boolean: aBool;
		yourself.!

byte: anInteger
	"answer a new instance wrapping anInteger"

	^ (self new)
		byte: anInteger;
		yourself.!

char: aCharacterOrInteger
	"answer a new instance wrapping aCharacterOrInteger"

	^ (self new)
		char: aCharacterOrInteger;
		yourself.!

defineFields
	"
	self compileDefinition.

	typedef union jvalue
	{
		jboolean z;
		jbyte b;
		jchar c;
		jshort s;
		jint i;
		jlong j;
		jfloat f;
		jdouble d;
		jobject l;
	} jvalue;

	but I've chosen to use the Java type names as the basic field names.  I've provided aliassing accessors
	in case anyone needs them.

	Note that all the fields are write-only so we don't try to read, say, Bools as JNIObjects"

	self
		defineField: #boolean type: BYTEField writeOnly offset: 0;
		defineField: #byte type: SBYTEField writeOnly offset: 0;
		defineField: #char type: WORDField writeOnly offset: 0;
		defineField: #short type: SWORDField writeOnly offset: 0;
		defineField: #int type: SDWORDField writeOnly offset: 0;
		defineField: #long type: SQWORDField writeOnly offset: 0;
		defineField: #float type: FLOATField writeOnly offset: 0;
		defineField: #double type: DOUBLEField writeOnly offset: 0;
		defineField: #object type: (PointerField type: JNIObject) beWriteOnly offset: 0;
		yourself.
!

double: aFloat
	"answer a new instance wrapping aFloat"

	^ (self new)
		double: aFloat;
		yourself.!

float: aFloat
	"answer a new instance wrapping aFloat"

	^ (self new)
		float: aFloat;
		yourself.!

int: anInteger
	"answer a new instance wrapping anInteger"

	^ (self new)
		int: anInteger;
		yourself.!

long: anInteger
	"answer a new instance wrapping anInteger"

	^ (self new)
		long: anInteger;
		yourself.!

object: aJNIObjectOrNil
	"answer a new instance wrapping aJNIObjectOrNil.
	(NB: although this isn't part of the 'CU Java Base' package, this method
	will work with JavaObject's too)"


	^ (self new)
		object: aJNIObjectOrNil;
		yourself.!

short: anInteger
	"answer a new instance wrapping anInteger"

	^ (self new)
		short: anInteger;
		yourself.! !
!JNIValue class categoriesFor: #boolean:!accessing!instance creation!public! !
!JNIValue class categoriesFor: #byte:!instance creation!public! !
!JNIValue class categoriesFor: #char:!accessing!instance creation!public! !
!JNIValue class categoriesFor: #defineFields!constants!public! !
!JNIValue class categoriesFor: #double:!instance creation!public! !
!JNIValue class categoriesFor: #float:!instance creation!public! !
!JNIValue class categoriesFor: #int:!instance creation!public! !
!JNIValue class categoriesFor: #long:!instance creation!public! !
!JNIValue class categoriesFor: #object:!accessing!instance creation!public! !
!JNIValue class categoriesFor: #short:!instance creation!public! !

JNIVTableStructure guid: (GUID fromString: '{1CBF73A5-F35F-4EDB-A4C2-1EDD670521EC}')!
JNIVTableStructure comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

One of these defines the layout of the vtable-like structures containing function pointers which are used by JNI.

The JNI pattern is as follows:

There is a structure of N function pointers, say it''s called FPBlock.

There is a typedef:
	typedef FPBlock *FPUser;

But the api then is all in term of (FPUser *) rather than FPUser -- i.e. there is an extra level of indirection.  The way to think of this is as if there were another class, FPInstance, and the definitions ran:

	struct FPBlock { ... as before ...};

	struct FPInstance { struct FPBlock *vtable };

	typedef FPInstance *FPInstancePtr;

And then the API used FPInstancePtr whereever it currently uses FPUser*.  I.e. we are implementing a poor-man''s vtable layout.

This class is used to represent the layout of the raw vtable.  Classes under JNIVTableClient correspond to FPInstance (above) -- i.e. they consist of a single pointer to a vtable.'!
!JNIVTableStructure categoriesForClass!Unclassified! !
!JNIVTableStructure class methodsFor!

buildDescriptorReturning: aReturnTypeString withArgs: anArgumentTypeString
	"private -- answer an ExternalDesciptor suitable for a function pointer of the given form.
.	Important note. we don't supply a valid function name since we are dealing with
	function pointers and we will later use hackery to associate methods compiled
	with the given signature with an address returned by JNI"

	^ ExternalDescriptor
		callingConvention: self callingConventionString
		returnType: aReturnTypeString trimBlanks
		argumentTypes: anArgumentTypeString trimBlanks.!

callingConventionString
	"private -- answer a String which describes our standard calling convention"

	^ 'stdcall:'.!

defineFields

	"note that we assume that all our fields are either DWORD fillers or function pointers.
	We generate a 'field' name for the function pointer field by mangling the function pointer
	name to remove the $:s"

	self functionPointerDefinitions do:
		[:each | self
				defineField: (self fieldAccessorFromFunctionName: (each at: 1))
				type: (self fieldTypeFromFunctionSignature: (each at: 2))].
		
!

definesNewFields
	"private -- overridden to take account of our definition of defineFields"

	^ super definesNewFields or: [self class includesSelector: #functionPointerDefinitions].
!

fieldAccessorFromFunctionName: aSymbol
	"private -- generate a mangled field accessor name for the correspding function name"

	^ aSymbol copyReplacing: $: withObject: $_.!

fieldTypeFromFunctionSignature: anExternalDescriptor
	"private -- generate a field type corresponding to anExternalDescriptor"

	^ anExternalDescriptor isNil
		ifTrue: [DWORDField new beReadOnly]
		ifFalse: [FunctionPointerField signature: anExternalDescriptor].!

functionPointerDefinitions
	"private -- answer a list of <Symbol ExternalDescriptor> pairs (Arrays of size two) which
	define the function pointers embedded in this structure.  If the signature is nil then a DWORD
	filler is used instead of defining a function pointer"

	"subclasses should extend this"
	^ OrderedCollection new.		! !
!JNIVTableStructure class categoriesFor: #buildDescriptorReturning:withArgs:!helpers!private! !
!JNIVTableStructure class categoriesFor: #callingConventionString!constants!private! !
!JNIVTableStructure class categoriesFor: #defineFields!constants!public! !
!JNIVTableStructure class categoriesFor: #definesNewFields!private!testing! !
!JNIVTableStructure class categoriesFor: #fieldAccessorFromFunctionName:!helpers!private! !
!JNIVTableStructure class categoriesFor: #fieldTypeFromFunctionSignature:!helpers!private! !
!JNIVTableStructure class categoriesFor: #functionPointerDefinitions!constants!public! !

JNIValueArray guid: (GUID fromString: '{87AF562D-6582-4F99-B5B1-E84FEFCE4C11}')!
JNIValueArray comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

One of these acts like a StructureArray of JNIValues, except that it also has direct write access to the typed fields at each index.  Hence inserting values into an argument array is significantly more efficient since temporary JNIValue wrapper objects are not created on each access.

On my machine:
sa := StructureArray length: 12 elementClass: JNIValue.
[(sa at: 6) int: 777] utime.	--> 4.0 (+/-0.07) microseconds

jva := JNIValueArray new: 12.
[jva intAt: 6 put: 777] utime.	--> 629 (+/-1) nanoseconds


'!
!JNIValueArray categoriesForClass!Unclassified! !
!JNIValueArray methodsFor!

at: anIndex
	"answer a JNIValue wrapping the element at anIndex"

	^ self elementClass fromAddress: self yourAddress + ((anIndex - 1) * 8).
!

at: anIndex put: aJNIValue
	"replace the data in our slot at anIndex by copying the data wrapped by aJNIValue"

	| offset |

	offset := (anIndex - 1) * 8.
	aJNIValue replaceBytesOf: bytes
			from: offset + 1
			to: offset + 8
			startingAt: 1.

	^ aJNIValue.
!

booleanAt: anIndex put: aBool
	"set the receiver's boolean slot at anIndex to the value of aBool encoded
	as 0 or 1"

	^ bytes byteAtOffset: anIndex-1*8 put: (aBool ifTrue: [1] ifFalse: [0]).!

byteAt: anIndex put: anInteger
	"set the receiver's byte slot at anIndex to anInteger"

	"#sbyteAtOffset:put: doesn't check, so we do it manually"
	(anInteger < -128 or: [anInteger > 127]) ifTrue: [self errorCantHold: anInteger].

	^ bytes sbyteAtOffset: anIndex-1*8 put: anInteger.!

charAt: anIndex put: aCharacterOrInteger
	"set the receiver's char slot at anIndex to the value of aCharOrInteger.
	Note that the integer value should fit in an unsigned 16bit word (i.e.
	be a valid Java char code)"

	^ bytes wordAtOffset: anIndex-1*8 put: aCharacterOrInteger.!

doubleAt: anIndex put: aFloat
	"set the receiver's double slot at anIndex to aFloat"

	^ bytes doubleAtOffset: anIndex-1*8 put: aFloat.!

elementClass
	"answer the class of <ExternalStructure> used to wrap our elements"

	^ JNIValue.!

floatAt: anIndex put: aFloat
	"set the receiver's float slot at anIndex to aFloat"

	^ bytes floatAtOffset: anIndex-1*8 put: aFloat.!

intAt: anIndex put: anInteger
	"set the receiver's int slot at anIndex to anInteger"

	^ bytes sdwordAtOffset: anIndex-1*8 put: anInteger.!

longAt: anIndex put: anInteger
	"set the receiver's long slot at anIndex to anInteger"

	^ bytes sqwordAtOffset: anIndex-1*8 put: anInteger.!

objectAt: anIndex put: aJNIObjectOrNil
	"set the receiver's 'object' slot at anIndex to aJNIObjectOrNil.
	(NB: although this isn't part of the 'CU Java Base' package, this will
	accept JavaObject's too)"

	^ bytes dwordAtOffset: anIndex-1*8 put: aJNIObjectOrNil asParameter yourAddress.!

shortAt: anIndex put: anInteger
	"set the receiver's short slot at anIndex to anInteger"

	^ bytes swordAtOffset: anIndex-1*8 put: anInteger.!

uncheckedAt: anInteger
	"needed to fulfill the (Private!!) contract forced upon us by the superclasses implementation
	of #do: etc"

	^ self at: anInteger.! !
!JNIValueArray categoriesFor: #at:!accessing!public! !
!JNIValueArray categoriesFor: #at:put:!accessing!public! !
!JNIValueArray categoriesFor: #booleanAt:put:!accessing!public! !
!JNIValueArray categoriesFor: #byteAt:put:!accessing!public! !
!JNIValueArray categoriesFor: #charAt:put:!accessing!public! !
!JNIValueArray categoriesFor: #doubleAt:put:!accessing!public! !
!JNIValueArray categoriesFor: #elementClass!constants!public! !
!JNIValueArray categoriesFor: #floatAt:put:!accessing!public! !
!JNIValueArray categoriesFor: #intAt:put:!accessing!public! !
!JNIValueArray categoriesFor: #longAt:put:!accessing!public! !
!JNIValueArray categoriesFor: #objectAt:put:!accessing!public! !
!JNIValueArray categoriesFor: #shortAt:put:!accessing!public! !
!JNIValueArray categoriesFor: #uncheckedAt:!accessing!private! !

!JNIValueArray class methodsFor!

elementSize
	"private -- answer the size of the receiver's elements"
	
	^ 8.	"JNIValue byteSize"!

initialize
	"private -- class-side initialisation.

		self initialize.
	"

	zero := self new: 0.!

uninitialize
	"private -- class-side tear-down.

		self uninitialize.
	"

	zero := nil.!

zero
	"answer the singleton zero-length instance"

	^ zero.! !
!JNIValueArray class categoriesFor: #elementSize!constants!private! !
!JNIValueArray class categoriesFor: #initialize!initializing!private! !
!JNIValueArray class categoriesFor: #uninitialize!initializing!private! !
!JNIValueArray class categoriesFor: #zero!instance creation!public! !

JNIInvokeInterface guid: (GUID fromString: '{47A983A2-78D3-44AF-AC10-F28BDF78E667}')!
JNIInvokeInterface comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Instances represent the layout of a JNI "_JNIInvokeInterface" structure.  Note that instances are not normally used at runtime, instead the vtable-like pointer class JavaVM is used.'!
!JNIInvokeInterface categoriesForClass!Unclassified! !
!JNIInvokeInterface methodsFor!

AttachCurrentThread_penv_threadArgs_
	"Answer the receiver's AttachCurrentThread_penv_threadArgs_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 16)!

AttachCurrentThreadAsDaemon_penv_threadArgs_
	"Answer the receiver's AttachCurrentThreadAsDaemon_penv_threadArgs_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 28)!

DestroyJavaVM_
	"Answer the receiver's DestroyJavaVM_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 12)!

DetachCurrentThread_
	"Answer the receiver's DetachCurrentThread_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 20)!

GetEnv_penv_version_
	"Answer the receiver's GetEnv_penv_version_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 24)!

Reserved0_
	"Answer the receiver's Reserved0_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 0)!

Reserved1_
	"Answer the receiver's Reserved1_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 4)!

Reserved2_
	"Answer the receiver's Reserved2_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 8)! !
!JNIInvokeInterface categoriesFor: #AttachCurrentThread_penv_threadArgs_!**compiled accessors**!public! !
!JNIInvokeInterface categoriesFor: #AttachCurrentThreadAsDaemon_penv_threadArgs_!**compiled accessors**!public! !
!JNIInvokeInterface categoriesFor: #DestroyJavaVM_!**compiled accessors**!public! !
!JNIInvokeInterface categoriesFor: #DetachCurrentThread_!**compiled accessors**!public! !
!JNIInvokeInterface categoriesFor: #GetEnv_penv_version_!**compiled accessors**!public! !
!JNIInvokeInterface categoriesFor: #Reserved0_!**compiled accessors**!public! !
!JNIInvokeInterface categoriesFor: #Reserved1_!**compiled accessors**!public! !
!JNIInvokeInterface categoriesFor: #Reserved2_!**compiled accessors**!public! !

!JNIInvokeInterface class methodsFor!

function: aSymbol
	"private -- answer a 2Array standing for a function pointer record that we don't use"

	^ Array with: aSymbol with: nil.!

function: aSymbol returning: aReturnTypeString withArgs: anArgumentsString
	"private -- answer a 2Array standing for a function pointer record with the given return
	type and arguments *plus* our standard first 'this' argument"

	| descriptor |

	descriptor := self
				buildDescriptorReturning: aReturnTypeString
				withArgs: ('JNIInvokeInterface* ' , anArgumentsString).

	self assert: [(aSymbol occurrencesOf: $:) = descriptor argumentCount].

	^ Array with: aSymbol with: descriptor.!

function: aSymbol withArgs: anArgumentsString
	"private -- answer a 2Array standing for a function pointer record returning 'sdword'
	and with given arguments *and* our standard first 'this' argument"

	^ self function: aSymbol returning: 'sdword' withArgs: anArgumentsString.!

functionPointerDefinitions
	"private -- answer a list of <Symbol ExternalDescriptor> pairs (Arrays of size two) which
	define the function pointers embedded in this structure.  If the signature is nil then a DWORD
	filler is used instead of defining a function pointer.

	struct JNIInvokeInterface_ {
		... normal fields up to here ...

		jint (JNICALL *DestroyJavaVM)(JavaVM *vm);
		jint (JNICALL *AttachCurrentThread)(JavaVM *vm, JNIEnv **penv, void *threadArgs);
		jint (JNICALL *DetachCurrentThread)(JavaVM *vm);

		---- added in JDK1.2 ----
		jint (JNICALL *GetEnv)(JavaVM *vm, JNIEnv **penv, jint version);

		---- added in J2SDK1.4 ---
		jint (JNICALL *AttachCurrentThreadAsDaemon)(JavaVM *vm, JNIEnv **penv, void *threadArgs);
	};"

	"subclasses should extend this"
	^ (super functionPointerDefinitions)
		add: (self function: #Reserved0:);
		add: (self function: #Reserved1:);
		add: (self function: #Reserved2:);

		add: (self function: #DestroyJavaVM: withArgs: '');
		add: (self function: #AttachCurrentThread:penv:threadArgs: withArgs: 'JNIEnv** void*');
		add: (self function: #DetachCurrentThread: withArgs: '');

		"============== added in JDK1.2 =============="
		add: (self function: #GetEnv:penv:version: withArgs: 'JNIEnv** sdword');

		"============== added in J2SDK1.4 =============="
		"(and not supported until I work out how to tell whether the buffer is actually this big -- bloody idiots!!"
		add: (self function: #AttachCurrentThreadAsDaemon:penv:threadArgs: withArgs: 'JNIEnv** void*');		

		yourself.
! !
!JNIInvokeInterface class categoriesFor: #function:!helpers!private! !
!JNIInvokeInterface class categoriesFor: #function:returning:withArgs:!helpers!private! !
!JNIInvokeInterface class categoriesFor: #function:withArgs:!helpers!private! !
!JNIInvokeInterface class categoriesFor: #functionPointerDefinitions!constants!public! !

JNINativeInterface guid: (GUID fromString: '{C1817DB2-628E-442B-AA98-DAD74AECD31F}')!
JNINativeInterface comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Instances represent the layout of a JNI "_JNIInvokeInterface" structure.  Note that instances are not normally used at runtime, instead the vtable-like pointer class JNIEnv is used.'!
!JNINativeInterface categoriesForClass!Unclassified! !
!JNINativeInterface methodsFor!

AllocObject_class_
	"Answer the receiver's AllocObject_class_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 108)!

CallBooleanMethod_
	"Answer the receiver's CallBooleanMethod_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 148)!

CallBooleanMethodA_obj_methodID_args_
	"Answer the receiver's CallBooleanMethodA_obj_methodID_args_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 156)!

CallBooleanMethodV_
	"Answer the receiver's CallBooleanMethodV_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 152)!

CallByteMethod_
	"Answer the receiver's CallByteMethod_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 160)!

CallByteMethodA_obj_methodID_args_
	"Answer the receiver's CallByteMethodA_obj_methodID_args_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 168)!

CallByteMethodV_
	"Answer the receiver's CallByteMethodV_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 164)!

CallCharMethod_
	"Answer the receiver's CallCharMethod_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 172)!

CallCharMethodA_obj_methodID_args_
	"Answer the receiver's CallCharMethodA_obj_methodID_args_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 180)!

CallCharMethodV_
	"Answer the receiver's CallCharMethodV_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 176)!

CallDoubleMethod_
	"Answer the receiver's CallDoubleMethod_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 232)!

CallDoubleMethodA_obj_methodID_args_
	"Answer the receiver's CallDoubleMethodA_obj_methodID_args_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 240)!

CallDoubleMethodV_
	"Answer the receiver's CallDoubleMethodV_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 236)!

CallFloatMethod_
	"Answer the receiver's CallFloatMethod_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 220)!

CallFloatMethodA_obj_methodID_args_
	"Answer the receiver's CallFloatMethodA_obj_methodID_args_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 228)!

CallFloatMethodV_
	"Answer the receiver's CallFloatMethodV_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 224)!

CallIntMethod_
	"Answer the receiver's CallIntMethod_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 196)!

CallIntMethodA_obj_methodID_args_
	"Answer the receiver's CallIntMethodA_obj_methodID_args_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 204)!

CallIntMethodV_
	"Answer the receiver's CallIntMethodV_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 200)!

CallLongMethod_
	"Answer the receiver's CallLongMethod_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 208)!

CallLongMethodA_obj_methodID_args_
	"Answer the receiver's CallLongMethodA_obj_methodID_args_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 216)!

CallLongMethodV_
	"Answer the receiver's CallLongMethodV_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 212)!

CallNonvirtualBooleanMethod_
	"Answer the receiver's CallNonvirtualBooleanMethod_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 272)!

CallNonvirtualBooleanMethodA_obj_class_methodID_args_
	"Answer the receiver's CallNonvirtualBooleanMethodA_obj_class_methodID_args_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 276)!

CallNonvirtualByteMethod_
	"Answer the receiver's CallNonvirtualByteMethod_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 280)!

CallNonvirtualByteMethodA_obj_class_methodID_args_
	"Answer the receiver's CallNonvirtualByteMethodA_obj_class_methodID_args_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 288)!

CallNonvirtualByteMethodV_
	"Answer the receiver's CallNonvirtualByteMethodV_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 284)!

CallNonvirtualCharMethod_
	"Answer the receiver's CallNonvirtualCharMethod_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 292)!

CallNonvirtualCharMethodA_obj_class_methodID_args_
	"Answer the receiver's CallNonvirtualCharMethodA_obj_class_methodID_args_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 300)!

CallNonvirtualCharMethodV_
	"Answer the receiver's CallNonvirtualCharMethodV_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 296)!

CallNonvirtualDoubleMethod_
	"Answer the receiver's CallNonvirtualDoubleMethod_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 352)!

CallNonvirtualDoubleMethodA_obj_class_methodID_args_
	"Answer the receiver's CallNonvirtualDoubleMethodA_obj_class_methodID_args_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 360)!

CallNonvirtualDoubleMethodV_
	"Answer the receiver's CallNonvirtualDoubleMethodV_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 356)!

CallNonvirtualFloatMethod_
	"Answer the receiver's CallNonvirtualFloatMethod_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 340)!

CallNonvirtualFloatMethodA_obj_class_methodID_args_
	"Answer the receiver's CallNonvirtualFloatMethodA_obj_class_methodID_args_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 348)!

CallNonvirtualFloatMethodV_
	"Answer the receiver's CallNonvirtualFloatMethodV_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 344)!

CallNonvirtualIntMethod_
	"Answer the receiver's CallNonvirtualIntMethod_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 316)!

CallNonvirtualIntMethodA_obj_class_methodID_args_
	"Answer the receiver's CallNonvirtualIntMethodA_obj_class_methodID_args_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 324)!

CallNonvirtualIntMethodV_
	"Answer the receiver's CallNonvirtualIntMethodV_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 320)!

CallNonvirtualLongMethod_
	"Answer the receiver's CallNonvirtualLongMethod_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 328)!

CallNonvirtualLongMethodA_obj_class_methodID_args_
	"Answer the receiver's CallNonvirtualLongMethodA_obj_class_methodID_args_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 336)!

CallNonvirtualLongMethodV_
	"Answer the receiver's CallNonvirtualLongMethodV_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 332)!

CallNonvirtualObjectMethod_
	"Answer the receiver's CallNonvirtualObjectMethod_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 256)!

CallNonvirtualObjectMethodA_obj_class_methodID_args_
	"Answer the receiver's CallNonvirtualObjectMethodA_obj_class_methodID_args_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 264)!

CallNonvirtualObjectMethodV_
	"Answer the receiver's CallNonvirtualObjectMethodV_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 260)!

CallNonvirtualShortMethod_
	"Answer the receiver's CallNonvirtualShortMethod_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 304)!

CallNonvirtualShortMethodA_obj_class_methodID_args_
	"Answer the receiver's CallNonvirtualShortMethodA_obj_class_methodID_args_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 312)!

CallNonvirtualShortMethodV_
	"Answer the receiver's CallNonvirtualShortMethodV_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 308)!

CallNonvirtualVoidMethod_
	"Answer the receiver's CallNonvirtualVoidMethod_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 364)!

CallNonvirtualVoidMethodA_obj_class_methodID_args_
	"Answer the receiver's CallNonvirtualVoidMethodA_obj_class_methodID_args_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 372)!

CallNonvirtualVoidMethodV_
	"Answer the receiver's CallNonvirtualVoidMethodV_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 368)!

CallObjectMethod_
	"Answer the receiver's CallObjectMethod_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 136)!

CallObjectMethodA_obj_methodID_args_
	"Answer the receiver's CallObjectMethodA_obj_methodID_args_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 144)!

CallObjectMethodV_
	"Answer the receiver's CallObjectMethodV_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 140)!

CallShortMethod_
	"Answer the receiver's CallShortMethod_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 184)!

CallShortMethodA_obj_methodID_args_
	"Answer the receiver's CallShortMethodA_obj_methodID_args_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 192)!

CallShortMethodV_
	"Answer the receiver's CallShortMethodV_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 188)!

CallStaticBooleanMethod_
	"Answer the receiver's CallStaticBooleanMethod_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 468)!

CallStaticBooleanMethodA_class_methodID_args_
	"Answer the receiver's CallStaticBooleanMethodA_class_methodID_args_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 476)!

CallStaticBooleanMethodV_
	"Answer the receiver's CallStaticBooleanMethodV_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 472)!

CallStaticByteMethod_
	"Answer the receiver's CallStaticByteMethod_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 480)!

CallStaticByteMethodA_class_methodID_args_
	"Answer the receiver's CallStaticByteMethodA_class_methodID_args_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 488)!

CallStaticByteMethodV_
	"Answer the receiver's CallStaticByteMethodV_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 484)!

CallStaticCharMethod_
	"Answer the receiver's CallStaticCharMethod_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 492)!

CallStaticCharMethodA_class_methodID_args_
	"Answer the receiver's CallStaticCharMethodA_class_methodID_args_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 500)!

CallStaticCharMethodV_
	"Answer the receiver's CallStaticCharMethodV_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 496)!

CallStaticDoubleMethod_
	"Answer the receiver's CallStaticDoubleMethod_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 552)!

CallStaticDoubleMethodA_class_methodID_args_
	"Answer the receiver's CallStaticDoubleMethodA_class_methodID_args_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 560)!

CallStaticDoubleMethodV_
	"Answer the receiver's CallStaticDoubleMethodV_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 556)!

CallStaticFloatMethod_
	"Answer the receiver's CallStaticFloatMethod_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 540)!

CallStaticFloatMethodA_class_methodID_args_
	"Answer the receiver's CallStaticFloatMethodA_class_methodID_args_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 548)!

CallStaticFloatMethodV_
	"Answer the receiver's CallStaticFloatMethodV_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 544)!

CallStaticIntMethod_
	"Answer the receiver's CallStaticIntMethod_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 516)!

CallStaticIntMethodA_class_methodID_args_
	"Answer the receiver's CallStaticIntMethodA_class_methodID_args_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 524)!

CallStaticIntMethodV_
	"Answer the receiver's CallStaticIntMethodV_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 520)!

CallStaticLongMethod_
	"Answer the receiver's CallStaticLongMethod_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 528)!

CallStaticLongMethodA_class_methodID_args_
	"Answer the receiver's CallStaticLongMethodA_class_methodID_args_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 536)!

CallStaticLongMethodV_
	"Answer the receiver's CallStaticLongMethodV_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 532)!

CallStaticObjectMethod_
	"Answer the receiver's CallStaticObjectMethod_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 456)!

CallStaticObjectMethodA_class_methodID_args_
	"Answer the receiver's CallStaticObjectMethodA_class_methodID_args_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 464)!

CallStaticObjectMethodV_
	"Answer the receiver's CallStaticObjectMethodV_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 460)!

CallStaticShortMethod_
	"Answer the receiver's CallStaticShortMethod_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 504)!

CallStaticShortMethodA_class_methodID_args_
	"Answer the receiver's CallStaticShortMethodA_class_methodID_args_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 512)!

CallStaticShortMethodV_
	"Answer the receiver's CallStaticShortMethodV_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 508)!

CallStaticVoidMethod_
	"Answer the receiver's CallStaticVoidMethod_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 564)!

CallStaticVoidMethodA_class_methodID_args_
	"Answer the receiver's CallStaticVoidMethodA_class_methodID_args_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 572)!

CallStaticVoidMethodV_
	"Answer the receiver's CallStaticVoidMethodV_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 568)!

CallVoidMethod_
	"Answer the receiver's CallVoidMethod_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 244)!

CallVoidMethodA_obj_methodID_args_
	"Answer the receiver's CallVoidMethodA_obj_methodID_args_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 252)!

CallVoidMethodV_
	"Answer the receiver's CallVoidMethodV_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 248)!

DefineClass_name_loader_buf_len_
	"Answer the receiver's DefineClass_name_loader_buf_len_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 20)!

DeleteGlobalRef_obj_
	"Answer the receiver's DeleteGlobalRef_obj_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 88)!

DeleteLocalRef_obj_
	"Answer the receiver's DeleteLocalRef_obj_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 92)!

DeleteWeakGlobalRef_ref_
	"Answer the receiver's DeleteWeakGlobalRef_ref_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 908)!

EnsureLocalCapacity_capacity_
	"Answer the receiver's EnsureLocalCapacity_capacity_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 104)!

ExceptionCheck_
	"Answer the receiver's ExceptionCheck_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 912)!

ExceptionClear_
	"Answer the receiver's ExceptionClear_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 68)!

ExceptionDescribe_
	"Answer the receiver's ExceptionDescribe_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 64)!

ExceptionOccurred_
	"Answer the receiver's ExceptionOccurred_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 60)!

FatalError_msg_
	"Answer the receiver's FatalError_msg_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 72)!

FindClass_name_
	"Answer the receiver's FindClass_name_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 24)!

FromReflectedField_field_
	"Answer the receiver's FromReflectedField_field_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 32)!

FromReflectedMethod_method_
	"Answer the receiver's FromReflectedMethod_method_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 28)!

GetArrayLength_array_
	"Answer the receiver's GetArrayLength_array_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 684)!

GetBooleanArrayElements_array_isCopy_
	"Answer the receiver's GetBooleanArrayElements_array_isCopy_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 732)!

GetBooleanArrayRegion_array_start_len_buf_
	"Answer the receiver's GetBooleanArrayRegion_array_start_len_buf_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 796)!

GetBooleanField_obj_fieldID_
	"Answer the receiver's GetBooleanField_obj_fieldID_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 384)!

GetByteArrayElements_array_isCopy_
	"Answer the receiver's GetByteArrayElements_array_isCopy_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 736)!

GetByteArrayRegion_array_start_len_buf_
	"Answer the receiver's GetByteArrayRegion_array_start_len_buf_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 800)!

GetByteField_obj_fieldID_
	"Answer the receiver's GetByteField_obj_fieldID_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 388)!

GetCharArrayElements_array_isCopy_
	"Answer the receiver's GetCharArrayElements_array_isCopy_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 740)!

GetCharArrayRegion_array_start_len_buf_
	"Answer the receiver's GetCharArrayRegion_array_start_len_buf_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 804)!

GetCharField_obj_fieldID_
	"Answer the receiver's GetCharField_obj_fieldID_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 392)!

GetDirectBufferAddress_buf_
	"Answer the receiver's GetDirectBufferAddress_buf_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 920)!

GetDirectBufferCapacity_buf_
	"Answer the receiver's GetDirectBufferCapacity_buf_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 924)!

GetDoubleArrayElements_array_isCopy_
	"Answer the receiver's GetDoubleArrayElements_array_isCopy_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 760)!

GetDoubleArrayRegion_array_start_len_buf_
	"Answer the receiver's GetDoubleArrayRegion_array_start_len_buf_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 824)!

GetDoubleField_obj_fieldID_
	"Answer the receiver's GetDoubleField_obj_fieldID_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 412)!

GetFieldID_class_name_sig_
	"Answer the receiver's GetFieldID_class_name_sig_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 376)!

GetFloatArrayElements_array_isCopy_
	"Answer the receiver's GetFloatArrayElements_array_isCopy_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 756)!

GetFloatArrayRegion_array_start_len_buf_
	"Answer the receiver's GetFloatArrayRegion_array_start_len_buf_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 820)!

GetFloatField_obj_fieldID_
	"Answer the receiver's GetFloatField_obj_fieldID_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 408)!

GetIntArrayElements_array_isCopy_
	"Answer the receiver's GetIntArrayElements_array_isCopy_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 748)!

GetIntArrayRegion_array_start_len_buf_
	"Answer the receiver's GetIntArrayRegion_array_start_len_buf_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 812)!

GetIntField_obj_fieldID_
	"Answer the receiver's GetIntField_obj_fieldID_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 400)!

GetJavaVM_pvm_
	"Answer the receiver's GetJavaVM_pvm_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 876)!

GetLongArrayElements_array_isCopy_
	"Answer the receiver's GetLongArrayElements_array_isCopy_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 752)!

GetLongArrayRegion_array_start_len_buf_
	"Answer the receiver's GetLongArrayRegion_array_start_len_buf_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 816)!

GetLongField_obj_fieldID_
	"Answer the receiver's GetLongField_obj_fieldID_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 404)!

GetMethodID_class_name_sig_
	"Answer the receiver's GetMethodID_class_name_sig_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 132)!

GetObjectArrayElement_array_index_
	"Answer the receiver's GetObjectArrayElement_array_index_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 692)!

GetObjectClass_obj_
	"Answer the receiver's GetObjectClass_obj_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 124)!

GetObjectField_obj_fieldID_
	"Answer the receiver's GetObjectField_obj_fieldID_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 380)!

GetPrimitiveArrayCritical_array_isCopy_
	"Answer the receiver's GetPrimitiveArrayCritical_array_isCopy_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 888)!

GetShortArrayElements_array_isCopy_
	"Answer the receiver's GetShortArrayElements_array_isCopy_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 744)!

GetShortArrayRegion_array_start_len_buf_
	"Answer the receiver's GetShortArrayRegion_array_start_len_buf_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 808)!

GetShortField_obj_fieldID_
	"Answer the receiver's GetShortField_obj_fieldID_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 396)!

GetStaticBooleanField_class_fieldID_
	"Answer the receiver's GetStaticBooleanField_class_fieldID_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 584)!

GetStaticByteField_class_fieldID_
	"Answer the receiver's GetStaticByteField_class_fieldID_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 588)!

GetStaticCharField_class_fieldID_
	"Answer the receiver's GetStaticCharField_class_fieldID_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 592)!

GetStaticDoubleField_class_fieldID_
	"Answer the receiver's GetStaticDoubleField_class_fieldID_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 612)!

GetStaticFieldID_class_name_sig_
	"Answer the receiver's GetStaticFieldID_class_name_sig_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 576)!

GetStaticFloatField_class_fieldID_
	"Answer the receiver's GetStaticFloatField_class_fieldID_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 608)!

GetStaticIntField_class_fieldID_
	"Answer the receiver's GetStaticIntField_class_fieldID_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 600)!

GetStaticLongField_class_fieldID_
	"Answer the receiver's GetStaticLongField_class_fieldID_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 604)!

GetStaticMethodID_class_name_sig_
	"Answer the receiver's GetStaticMethodID_class_name_sig_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 452)!

GetStaticObjectField_class_fieldID_
	"Answer the receiver's GetStaticObjectField_class_fieldID_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 580)!

GetStaticShortField_class_fieldID_
	"Answer the receiver's GetStaticShortField_class_fieldID_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 596)!

GetStringChars_str_isCopy_
	"Answer the receiver's GetStringChars_str_isCopy_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 660)!

GetStringCritical_str_isCopy_
	"Answer the receiver's GetStringCritical_str_isCopy_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 896)!

GetStringLength_str_
	"Answer the receiver's GetStringLength_str_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 656)!

GetStringRegion_str_start_len_buf_
	"Answer the receiver's GetStringRegion_str_start_len_buf_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 880)!

GetStringUTFChars_str_isCopy_
	"Answer the receiver's GetStringUTFChars_str_isCopy_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 676)!

GetStringUTFLength_str_
	"Answer the receiver's GetStringUTFLength_str_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 672)!

GetStringUTFRegion_str_start_len_buf_
	"Answer the receiver's GetStringUTFRegion_str_start_len_buf_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 884)!

GetSuperclass_sub_
	"Answer the receiver's GetSuperclass_sub_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 40)!

GetVersion_
	"Answer the receiver's GetVersion_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 16)!

IsAssignableFrom_sub_sup_
	"Answer the receiver's IsAssignableFrom_sub_sup_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 44)!

IsInstanceOf_obj_class_
	"Answer the receiver's IsInstanceOf_obj_class_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 128)!

IsSameObject_obj_obj_
	"Answer the receiver's IsSameObject_obj_obj_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 96)!

MonitorEnter_obj_
	"Answer the receiver's MonitorEnter_obj_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 868)!

MonitorExit_obj_
	"Answer the receiver's MonitorExit_obj_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 872)!

NewBooleanArray_len_
	"Answer the receiver's NewBooleanArray_len_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 700)!

NewByteArray_len_
	"Answer the receiver's NewByteArray_len_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 704)!

NewCharArray_len_
	"Answer the receiver's NewCharArray_len_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 708)!

NewDirectByteBuffer_address_capacity_
	"Answer the receiver's NewDirectByteBuffer_address_capacity_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 916)!

NewDoubleArray_len_
	"Answer the receiver's NewDoubleArray_len_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 728)!

NewFloatArray_len_
	"Answer the receiver's NewFloatArray_len_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 724)!

NewGlobalRef_obj_
	"Answer the receiver's NewGlobalRef_obj_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 84)!

NewIntArray_len_
	"Answer the receiver's NewIntArray_len_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 716)!

NewLocalRef_obj_
	"Answer the receiver's NewLocalRef_obj_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 100)!

NewLongArray_len_
	"Answer the receiver's NewLongArray_len_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 720)!

NewObject_
	"Answer the receiver's NewObject_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 112)!

NewObjectA_class_methodID_args_
	"Answer the receiver's NewObjectA_class_methodID_args_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 120)!

NewObjectArray_len_class_init_
	"Answer the receiver's NewObjectArray_len_class_init_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 688)!

NewObjectV_
	"Answer the receiver's NewObjectV_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 116)!

NewShortArray_len_
	"Answer the receiver's NewShortArray_len_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 712)!

NewString_unicode_len_
	"Answer the receiver's NewString_unicode_len_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 652)!

NewStringUTF_utf_
	"Answer the receiver's NewStringUTF_utf_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 668)!

NewWeakGlobalRef_obj_
	"Answer the receiver's NewWeakGlobalRef_obj_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 904)!

PopLocalFrame_result_
	"Answer the receiver's PopLocalFrame_result_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 80)!

PushLocalFrame_capacity_
	"Answer the receiver's PushLocalFrame_capacity_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 76)!

RegisterNatives_class_methods_nMethods_
	"Answer the receiver's RegisterNatives_class_methods_nMethods_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 860)!

ReleaseBooleanArrayElements_array_elems_mode_
	"Answer the receiver's ReleaseBooleanArrayElements_array_elems_mode_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 764)!

ReleaseByteArrayElements_array_elems_mode_
	"Answer the receiver's ReleaseByteArrayElements_array_elems_mode_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 768)!

ReleaseCharArrayElements_array_elems_mode_
	"Answer the receiver's ReleaseCharArrayElements_array_elems_mode_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 772)!

ReleaseDoubleArrayElements_array_elems_mode_
	"Answer the receiver's ReleaseDoubleArrayElements_array_elems_mode_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 792)!

ReleaseFloatArrayElements_array_elems_mode_
	"Answer the receiver's ReleaseFloatArrayElements_array_elems_mode_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 788)!

ReleaseIntArrayElements_array_elems_mode_
	"Answer the receiver's ReleaseIntArrayElements_array_elems_mode_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 780)!

ReleaseLongArrayElements_array_elems_mode_
	"Answer the receiver's ReleaseLongArrayElements_array_elems_mode_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 784)!

ReleasePrimitiveArrayCritical_array_carray_mode_
	"Answer the receiver's ReleasePrimitiveArrayCritical_array_carray_mode_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 892)!

ReleaseShortArrayElements_array_elems_mode_
	"Answer the receiver's ReleaseShortArrayElements_array_elems_mode_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 776)!

ReleaseStringChars_str_chars_
	"Answer the receiver's ReleaseStringChars_str_chars_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 664)!

ReleaseStringCritical_str_cstring_
	"Answer the receiver's ReleaseStringCritical_str_cstring_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 900)!

ReleaseStringUTFChars_str_chars_
	"Answer the receiver's ReleaseStringUTFChars_str_chars_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 680)!

Reserved0_
	"Answer the receiver's Reserved0_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 0)!

Reserved1_
	"Answer the receiver's Reserved1_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 4)!

Reserved2_
	"Answer the receiver's Reserved2_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 8)!

Reserved3_
	"Answer the receiver's Reserved3_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 12)!

SetBooleanArrayRegion_array_start_len_buf_
	"Answer the receiver's SetBooleanArrayRegion_array_start_len_buf_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 828)!

SetBooleanField_obj_fieldID_val_
	"Answer the receiver's SetBooleanField_obj_fieldID_val_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 420)!

SetByteArrayRegion_array_start_len_buf_
	"Answer the receiver's SetByteArrayRegion_array_start_len_buf_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 832)!

SetByteField_obj_fieldID_val_
	"Answer the receiver's SetByteField_obj_fieldID_val_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 424)!

SetCharArrayRegion_array_start_len_buf_
	"Answer the receiver's SetCharArrayRegion_array_start_len_buf_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 836)!

SetCharField_obj_fieldID_val_
	"Answer the receiver's SetCharField_obj_fieldID_val_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 428)!

SetDoubleArrayRegion_array_start_len_buf_
	"Answer the receiver's SetDoubleArrayRegion_array_start_len_buf_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 856)!

SetDoubleField_obj_fieldID_val_
	"Answer the receiver's SetDoubleField_obj_fieldID_val_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 448)!

SetFloatArrayRegion_array_start_len_buf_
	"Answer the receiver's SetFloatArrayRegion_array_start_len_buf_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 852)!

SetFloatField_obj_fieldID_val_
	"Answer the receiver's SetFloatField_obj_fieldID_val_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 444)!

SetIntArrayRegion_array_start_len_buf_
	"Answer the receiver's SetIntArrayRegion_array_start_len_buf_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 844)!

SetIntField_obj_fieldID_val_
	"Answer the receiver's SetIntField_obj_fieldID_val_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 436)!

SetLongArrayRegion_array_start_len_buf_
	"Answer the receiver's SetLongArrayRegion_array_start_len_buf_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 848)!

SetLongField_obj_fieldID_val_
	"Answer the receiver's SetLongField_obj_fieldID_val_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 440)!

SetObjectArrayElement_array_index_val_
	"Answer the receiver's SetObjectArrayElement_array_index_val_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 696)!

SetObjectField_obj_fieldID_val_
	"Answer the receiver's SetObjectField_obj_fieldID_val_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 416)!

SetShortArrayRegion_array_start_len_buf_
	"Answer the receiver's SetShortArrayRegion_array_start_len_buf_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 840)!

SetShortField_obj_fieldID_val_
	"Answer the receiver's SetShortField_obj_fieldID_val_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 432)!

SetStaticBooleanField_class_fieldID_val_
	"Answer the receiver's SetStaticBooleanField_class_fieldID_val_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 620)!

SetStaticByteField_class_fieldID_val_
	"Answer the receiver's SetStaticByteField_class_fieldID_val_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 624)!

SetStaticCharField_class_fieldID_val_
	"Answer the receiver's SetStaticCharField_class_fieldID_val_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 628)!

SetStaticDoubleField_class_fieldID_val_
	"Answer the receiver's SetStaticDoubleField_class_fieldID_val_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 648)!

SetStaticFloatField_class_fieldID_val_
	"Answer the receiver's SetStaticFloatField_class_fieldID_val_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 644)!

SetStaticIntField_class_fieldID_val_
	"Answer the receiver's SetStaticIntField_class_fieldID_val_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 636)!

SetStaticLongField_class_fieldID_val_
	"Answer the receiver's SetStaticLongField_class_fieldID_val_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 640)!

SetStaticObjectField_class_fieldID_val_
	"Answer the receiver's SetStaticObjectField_class_fieldID_val_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 616)!

SetStaticShortField_class_fieldID_val_
	"Answer the receiver's SetStaticShortField_class_fieldID_val_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 632)!

Throw_obj_
	"Answer the receiver's Throw_obj_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 52)!

ThrowNew_class_msg_
	"Answer the receiver's ThrowNew_class_msg_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 56)!

ToReflectedField_class_fieldID_isStatic_
	"Answer the receiver's ToReflectedField_class_fieldID_isStatic_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 48)!

ToReflectedMethod_class_methodID_isStatic_
	"Answer the receiver's ToReflectedMethod_class_methodID_isStatic_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 36)!

UnregisterNatives_class_
	"Answer the receiver's UnregisterNatives_class_ field as a Smalltalk object."

	^(bytes dwordAtOffset: 864)! !
!JNINativeInterface categoriesFor: #AllocObject_class_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallBooleanMethod_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallBooleanMethodA_obj_methodID_args_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallBooleanMethodV_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallByteMethod_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallByteMethodA_obj_methodID_args_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallByteMethodV_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallCharMethod_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallCharMethodA_obj_methodID_args_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallCharMethodV_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallDoubleMethod_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallDoubleMethodA_obj_methodID_args_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallDoubleMethodV_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallFloatMethod_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallFloatMethodA_obj_methodID_args_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallFloatMethodV_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallIntMethod_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallIntMethodA_obj_methodID_args_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallIntMethodV_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallLongMethod_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallLongMethodA_obj_methodID_args_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallLongMethodV_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallNonvirtualBooleanMethod_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallNonvirtualBooleanMethodA_obj_class_methodID_args_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallNonvirtualByteMethod_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallNonvirtualByteMethodA_obj_class_methodID_args_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallNonvirtualByteMethodV_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallNonvirtualCharMethod_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallNonvirtualCharMethodA_obj_class_methodID_args_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallNonvirtualCharMethodV_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallNonvirtualDoubleMethod_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallNonvirtualDoubleMethodA_obj_class_methodID_args_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallNonvirtualDoubleMethodV_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallNonvirtualFloatMethod_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallNonvirtualFloatMethodA_obj_class_methodID_args_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallNonvirtualFloatMethodV_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallNonvirtualIntMethod_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallNonvirtualIntMethodA_obj_class_methodID_args_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallNonvirtualIntMethodV_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallNonvirtualLongMethod_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallNonvirtualLongMethodA_obj_class_methodID_args_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallNonvirtualLongMethodV_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallNonvirtualObjectMethod_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallNonvirtualObjectMethodA_obj_class_methodID_args_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallNonvirtualObjectMethodV_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallNonvirtualShortMethod_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallNonvirtualShortMethodA_obj_class_methodID_args_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallNonvirtualShortMethodV_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallNonvirtualVoidMethod_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallNonvirtualVoidMethodA_obj_class_methodID_args_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallNonvirtualVoidMethodV_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallObjectMethod_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallObjectMethodA_obj_methodID_args_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallObjectMethodV_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallShortMethod_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallShortMethodA_obj_methodID_args_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallShortMethodV_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallStaticBooleanMethod_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallStaticBooleanMethodA_class_methodID_args_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallStaticBooleanMethodV_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallStaticByteMethod_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallStaticByteMethodA_class_methodID_args_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallStaticByteMethodV_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallStaticCharMethod_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallStaticCharMethodA_class_methodID_args_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallStaticCharMethodV_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallStaticDoubleMethod_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallStaticDoubleMethodA_class_methodID_args_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallStaticDoubleMethodV_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallStaticFloatMethod_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallStaticFloatMethodA_class_methodID_args_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallStaticFloatMethodV_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallStaticIntMethod_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallStaticIntMethodA_class_methodID_args_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallStaticIntMethodV_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallStaticLongMethod_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallStaticLongMethodA_class_methodID_args_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallStaticLongMethodV_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallStaticObjectMethod_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallStaticObjectMethodA_class_methodID_args_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallStaticObjectMethodV_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallStaticShortMethod_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallStaticShortMethodA_class_methodID_args_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallStaticShortMethodV_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallStaticVoidMethod_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallStaticVoidMethodA_class_methodID_args_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallStaticVoidMethodV_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallVoidMethod_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallVoidMethodA_obj_methodID_args_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #CallVoidMethodV_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #DefineClass_name_loader_buf_len_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #DeleteGlobalRef_obj_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #DeleteLocalRef_obj_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #DeleteWeakGlobalRef_ref_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #EnsureLocalCapacity_capacity_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #ExceptionCheck_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #ExceptionClear_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #ExceptionDescribe_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #ExceptionOccurred_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #FatalError_msg_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #FindClass_name_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #FromReflectedField_field_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #FromReflectedMethod_method_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #GetArrayLength_array_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #GetBooleanArrayElements_array_isCopy_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #GetBooleanArrayRegion_array_start_len_buf_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #GetBooleanField_obj_fieldID_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #GetByteArrayElements_array_isCopy_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #GetByteArrayRegion_array_start_len_buf_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #GetByteField_obj_fieldID_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #GetCharArrayElements_array_isCopy_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #GetCharArrayRegion_array_start_len_buf_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #GetCharField_obj_fieldID_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #GetDirectBufferAddress_buf_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #GetDirectBufferCapacity_buf_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #GetDoubleArrayElements_array_isCopy_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #GetDoubleArrayRegion_array_start_len_buf_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #GetDoubleField_obj_fieldID_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #GetFieldID_class_name_sig_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #GetFloatArrayElements_array_isCopy_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #GetFloatArrayRegion_array_start_len_buf_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #GetFloatField_obj_fieldID_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #GetIntArrayElements_array_isCopy_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #GetIntArrayRegion_array_start_len_buf_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #GetIntField_obj_fieldID_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #GetJavaVM_pvm_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #GetLongArrayElements_array_isCopy_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #GetLongArrayRegion_array_start_len_buf_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #GetLongField_obj_fieldID_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #GetMethodID_class_name_sig_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #GetObjectArrayElement_array_index_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #GetObjectClass_obj_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #GetObjectField_obj_fieldID_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #GetPrimitiveArrayCritical_array_isCopy_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #GetShortArrayElements_array_isCopy_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #GetShortArrayRegion_array_start_len_buf_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #GetShortField_obj_fieldID_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #GetStaticBooleanField_class_fieldID_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #GetStaticByteField_class_fieldID_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #GetStaticCharField_class_fieldID_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #GetStaticDoubleField_class_fieldID_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #GetStaticFieldID_class_name_sig_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #GetStaticFloatField_class_fieldID_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #GetStaticIntField_class_fieldID_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #GetStaticLongField_class_fieldID_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #GetStaticMethodID_class_name_sig_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #GetStaticObjectField_class_fieldID_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #GetStaticShortField_class_fieldID_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #GetStringChars_str_isCopy_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #GetStringCritical_str_isCopy_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #GetStringLength_str_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #GetStringRegion_str_start_len_buf_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #GetStringUTFChars_str_isCopy_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #GetStringUTFLength_str_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #GetStringUTFRegion_str_start_len_buf_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #GetSuperclass_sub_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #GetVersion_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #IsAssignableFrom_sub_sup_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #IsInstanceOf_obj_class_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #IsSameObject_obj_obj_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #MonitorEnter_obj_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #MonitorExit_obj_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #NewBooleanArray_len_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #NewByteArray_len_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #NewCharArray_len_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #NewDirectByteBuffer_address_capacity_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #NewDoubleArray_len_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #NewFloatArray_len_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #NewGlobalRef_obj_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #NewIntArray_len_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #NewLocalRef_obj_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #NewLongArray_len_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #NewObject_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #NewObjectA_class_methodID_args_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #NewObjectArray_len_class_init_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #NewObjectV_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #NewShortArray_len_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #NewString_unicode_len_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #NewStringUTF_utf_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #NewWeakGlobalRef_obj_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #PopLocalFrame_result_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #PushLocalFrame_capacity_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #RegisterNatives_class_methods_nMethods_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #ReleaseBooleanArrayElements_array_elems_mode_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #ReleaseByteArrayElements_array_elems_mode_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #ReleaseCharArrayElements_array_elems_mode_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #ReleaseDoubleArrayElements_array_elems_mode_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #ReleaseFloatArrayElements_array_elems_mode_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #ReleaseIntArrayElements_array_elems_mode_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #ReleaseLongArrayElements_array_elems_mode_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #ReleasePrimitiveArrayCritical_array_carray_mode_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #ReleaseShortArrayElements_array_elems_mode_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #ReleaseStringChars_str_chars_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #ReleaseStringCritical_str_cstring_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #ReleaseStringUTFChars_str_chars_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #Reserved0_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #Reserved1_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #Reserved2_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #Reserved3_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #SetBooleanArrayRegion_array_start_len_buf_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #SetBooleanField_obj_fieldID_val_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #SetByteArrayRegion_array_start_len_buf_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #SetByteField_obj_fieldID_val_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #SetCharArrayRegion_array_start_len_buf_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #SetCharField_obj_fieldID_val_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #SetDoubleArrayRegion_array_start_len_buf_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #SetDoubleField_obj_fieldID_val_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #SetFloatArrayRegion_array_start_len_buf_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #SetFloatField_obj_fieldID_val_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #SetIntArrayRegion_array_start_len_buf_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #SetIntField_obj_fieldID_val_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #SetLongArrayRegion_array_start_len_buf_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #SetLongField_obj_fieldID_val_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #SetObjectArrayElement_array_index_val_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #SetObjectField_obj_fieldID_val_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #SetShortArrayRegion_array_start_len_buf_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #SetShortField_obj_fieldID_val_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #SetStaticBooleanField_class_fieldID_val_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #SetStaticByteField_class_fieldID_val_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #SetStaticCharField_class_fieldID_val_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #SetStaticDoubleField_class_fieldID_val_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #SetStaticFloatField_class_fieldID_val_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #SetStaticIntField_class_fieldID_val_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #SetStaticLongField_class_fieldID_val_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #SetStaticObjectField_class_fieldID_val_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #SetStaticShortField_class_fieldID_val_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #Throw_obj_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #ThrowNew_class_msg_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #ToReflectedField_class_fieldID_isStatic_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #ToReflectedMethod_class_methodID_isStatic_!**compiled accessors**!public! !
!JNINativeInterface categoriesFor: #UnregisterNatives_class_!**compiled accessors**!public! !

!JNINativeInterface class methodsFor!

function: aSymbol
	"private -- answer a 2Array standing for a function pointer record that we don't use"

	^ Array with: aSymbol with: nil.!

function: aSymbol returning: aReturnTypeString
	"private -- answer a 2Array standing for a function pointer record with the given return
	type and only the standard single 'this' argument"

	^ self function: aSymbol returning: aReturnTypeString withArgs: ''.!

function: aSymbol returning: aReturnTypeString withArgs: anArgumentsString
	"private -- answer a 2Array standing for a function pointer record with the given return
	type and arguments *plus* our standard first 'this' argument"

	| descriptor |

	descriptor := self
			buildDescriptorReturning: aReturnTypeString
			withArgs: ('JNINativeInterface** ' , anArgumentsString).

	self assert: [(aSymbol occurrencesOf: $:) = descriptor argumentCount].

	^ Array with: aSymbol with: descriptor.!

functionPointerDefinitions
	"private -- answer a list of <Symbol ExternalDescriptor> pairs (Arrays of size two) which
	define the function pointers embedded in this structure.  If the signature is nil then a DWORD
	filler is used instead of defining a function pointer.

	Personal note: if you don't think it was dull setting all this crap up, then you should seek medical advice.

	"

	^ (super functionPointerDefinitions)

		add: (self function: #Reserved0:);
		add: (self function: #Reserved1:);
		add: (self function: #Reserved2:);
		add: (self function: #Reserved3:);

		"
		jint (JNICALL *GetVersion)(JNIEnv *env);
		"
		add: (self function: #GetVersion: returning: 'sdword');


		"
		jclass (JNICALL *DefineClass)(JNIEnv *env, const char *name, jobject loader, const jbyte *buf, jsize len);
		jclass (JNICALL *FindClass)(JNIEnv *env, const char *name);
		"
		add: (self function: #DefineClass:name:loader:buf:len: returning: 'JNIClass' withArgs: 'char* JNIObject byte* sdword');
		add: (self function: #FindClass:name: returning: 'JNIClass' withArgs: 'char*');

	
		"
		jmethodID (JNICALL *FromReflectedMethod)(JNIEnv *env, jobject method);
		jfieldID (JNICALL *FromReflectedField)(JNIEnv *env, jobject field);
		"
		add: (self function: #FromReflectedMethod:method: returning: 'JNIMethodID' withArgs: 'JNIObject');
		add: (self function: #FromReflectedField:field: returning: 'JNIFieldID' withArgs: 'JNIObject');


		"
		jobject (JNICALL *ToReflectedMethod)(JNIEnv *env, jclass class, jmethodID methodID, jboolean isStatic);
		"
		add: (self function: #ToReflectedMethod:class:methodID:isStatic: returning: 'JNIObject' withArgs: 'JNIClass JNIMethodID byte');


		"
		jclass (JNICALL *GetSuperclass)(JNIEnv *env, jclass sub);
		jboolean (JNICALL *IsAssignableFrom)(JNIEnv *env, jclass sub, jclass sup);
		"
		add: (self function: #GetSuperclass:sub: returning: 'JNIClass' withArgs: 'JNIClass');
		add: (self function: #IsAssignableFrom:sub:sup: returning: 'byte' withArgs: 'JNIClass JNIClass');


		"
		jobject (JNICALL *ToReflectedField)(JNIEnv *env, jclass class, jfieldID fieldID, jboolean isStatic);
		"
		add: (self function: #ToReflectedField:class:fieldID:isStatic: returning: 'JNIObject' withArgs: 'JNIClass JNIFieldID byte');


		"
		jint (JNICALL *Throw)(JNIEnv *env, jthrowable obj);
		jint (JNICALL *ThrowNew)(JNIEnv *env, jclass class, const char *msg);
		jthrowable (JNICALL *ExceptionOccurred)(JNIEnv *env);
		void (JNICALL *ExceptionDescribe)(JNIEnv *env);
		void (JNICALL *ExceptionClear)(JNIEnv *env);
		void (JNICALL *FatalError)(JNIEnv *env, const char *msg);
		"
		add: (self function: #Throw:obj: returning: 'sdword' withArgs: 'JNIThrowable');
		add: (self function: #ThrowNew:class:msg: returning: 'sdword' withArgs: 'JNIClass char*');
		add: (self function: #ExceptionOccurred: returning: 'JNIThrowable');
		add: (self function: #ExceptionDescribe: returning: 'void');
		add: (self function: #ExceptionClear: returning: 'void');
		add: (self function: #FatalError:msg: returning: 'void' withArgs: 'char*');


		"
		jint (JNICALL *PushLocalFrame)(JNIEnv *env, jint capacity);
		jobject (JNICALL *PopLocalFrame)(JNIEnv *env, jobject result);
		"
		add: (self function: #PushLocalFrame:capacity: returning: 'sdword' withArgs: 'sdword');
		add: (self function: #PopLocalFrame:result: returning: 'JNIObject' withArgs: 'JNIObject');


		"
		jobject (JNICALL *NewGlobalRef)(JNIEnv *env, jobject lobj);
		void (JNICALL *DeleteGlobalRef)(JNIEnv *env, jobject gref);
		void (JNICALL *DeleteLocalRef)(JNIEnv *env, jobject obj);
		jboolean (JNICALL *IsSameObject)(JNIEnv *env, jobject obj1, jobject obj2);
		jobject (JNICALL *NewLocalRef)(JNIEnv *env, jobject ref);
		jint (JNICALL *EnsureLocalCapacity)(JNIEnv *env, jint capacity);
		"
		add: (self function: #NewGlobalRef:obj: returning: 'JNIObjectG' withArgs: 'JNIObject');			"will also work for globals"
		add: (self function: #DeleteGlobalRef:obj: returning: 'void' withArgs: 'JNIObjectG');
		add: (self function: #DeleteLocalRef:obj: returning: 'void' withArgs: 'JNIObject');
		add: (self function: #IsSameObject:obj:obj: returning: 'byte' withArgs: 'JNIObject JNIObject');	"will also work for globals"
		add: (self function: #NewLocalRef:obj: returning: 'JNIObject' withArgs: 'JNIObjectG');			"will also work for locals"
		add: (self function: #EnsureLocalCapacity:capacity: returning: 'sdword' withArgs: 'sdword');

		"
		jobject (JNICALL *AllocObject)(JNIEnv *env, jclass class);
		jobject (JNICALL *NewObject)(JNIEnv *env, jclass class, jmethodID methodID, ...);
		jobject (JNICALL *NewObjectV)(JNIEnv *env, jclass class, jmethodID methodID, va_list args);
		jobject (JNICALL *NewObjectA)(JNIEnv *env, jclass class, jmethodID methodID, jvalue *args);
		"
		add: (self function: #AllocObject:class: returning: 'JNIObject' withArgs: 'JNIClass');
		add: (self function: #NewObject:);
		add: (self function: #NewObjectV:);
		add: (self function: #NewObjectA:class:methodID:args: returning: 'JNIObject' withArgs: 'JNIClass JNIMethodID JNIValueArray*');


		"
		jclass (JNICALL *GetObjectClass)(JNIEnv *env, jobject obj);
		jboolean (JNICALL *IsInstanceOf)(JNIEnv *env, jobject obj, jclass class);
		"
		add: (self function: #GetObjectClass:obj: returning: 'JNIClass' withArgs: 'JNIObject');
		add: (self function: #IsInstanceOf:obj:class: returning: 'byte' withArgs: 'JNIObject JNIClass');


		"
		jmethodID (JNICALL *GetMethodID)(JNIEnv *env, jclass class, const char *name, const char *sig);
		"
		add: (self function: #GetMethodID:class:name:sig: returning: 'JNIMethodID' withArgs: 'JNIClass char* char*');


		"
		jobject (JNICALL *CallObjectMethod)(JNIEnv *env, jobject obj, jmethodID methodID, ...);
		jobject (JNICALL *CallObjectMethodV)(JNIEnv *env, jobject obj, jmethodID methodID, va_list args);
		jobject (JNICALL *CallObjectMethodA)(JNIEnv *env, jobject obj, jmethodID methodID, jvalue * args);
		"
		add: (self function: #CallObjectMethod:);
		add: (self function: #CallObjectMethodV:);
		add: (self function: #CallObjectMethodA:obj:methodID:args: returning: 'JNIObject' withArgs: 'JNIObject JNIMethodID JNIValueArray*');


		"
		jboolean (JNICALL *CallBooleanMethod)(JNIEnv *env, jobject obj, jmethodID methodID, ...);
		jboolean (JNICALL *CallBooleanMethodV)(JNIEnv *env, jobject obj, jmethodID methodID, va_list args);
		jboolean (JNICALL *CallBooleanMethodA)(JNIEnv *env, jobject obj, jmethodID methodID, jvalue * args);
		"
		add: (self function: #CallBooleanMethod:);
		add: (self function: #CallBooleanMethodV:);
		add: (self function: #CallBooleanMethodA:obj:methodID:args: returning: 'byte' withArgs: 'JNIObject JNIMethodID JNIValueArray*');


		"
		jbyte (JNICALL *CallByteMethod)(JNIEnv *env, jobject obj, jmethodID methodID, ...);
		jbyte (JNICALL *CallByteMethodV)(JNIEnv *env, jobject obj, jmethodID methodID, va_list args);
		jbyte (JNICALL *CallByteMethodA)(JNIEnv *env, jobject obj, jmethodID methodID, jvalue *args);
		"
		add: (self function: #CallByteMethod:);
		add: (self function: #CallByteMethodV:);
		add: (self function: #CallByteMethodA:obj:methodID:args: returning: 'sbyte' withArgs: 'JNIObject JNIMethodID JNIValueArray*');


		"
		jchar (JNICALL *CallCharMethod)(JNIEnv *env, jobject obj, jmethodID methodID, ...);
		jchar (JNICALL *CallCharMethodV)(JNIEnv *env, jobject obj, jmethodID methodID, va_list args);
		jchar (JNICALL *CallCharMethodA)(JNIEnv *env, jobject obj, jmethodID methodID, jvalue *args);
		"
		add: (self function: #CallCharMethod:);
		add: (self function: #CallCharMethodV:);
		add: (self function: #CallCharMethodA:obj:methodID:args: returning: 'word' withArgs: 'JNIObject JNIMethodID JNIValueArray*');


		"
		jshort (JNICALL *CallShortMethod)(JNIEnv *env, jobject obj, jmethodID methodID, ...);
		jshort (JNICALL *CallShortMethodV)(JNIEnv *env, jobject obj, jmethodID methodID, va_list args);
		jshort (JNICALL *CallShortMethodA)(JNIEnv *env, jobject obj, jmethodID methodID, jvalue *args);
		"
		add: (self function: #CallShortMethod:);
		add: (self function: #CallShortMethodV:);
		add: (self function: #CallShortMethodA:obj:methodID:args: returning: 'sword' withArgs: 'JNIObject JNIMethodID JNIValueArray*');


		"
		jint (JNICALL *CallIntMethod)(JNIEnv *env, jobject obj, jmethodID methodID, ...);
		jint (JNICALL *CallIntMethodV)(JNIEnv *env, jobject obj, jmethodID methodID, va_list args);
		jint (JNICALL *CallIntMethodA)(JNIEnv *env, jobject obj, jmethodID methodID, jvalue *args);
		"
		add: (self function: #CallIntMethod:);
		add: (self function: #CallIntMethodV:);
		add: (self function: #CallIntMethodA:obj:methodID:args: returning: 'sdword' withArgs: 'JNIObject JNIMethodID JNIValueArray*');


		"
		jlong (JNICALL *CallLongMethod)(JNIEnv *env, jobject obj, jmethodID methodID, ...);
		jlong (JNICALL *CallLongMethodV)(JNIEnv *env, jobject obj, jmethodID methodID, va_list args);
		jlong (JNICALL *CallLongMethodA)(JNIEnv *env, jobject obj, jmethodID methodID, jvalue *args);
		"
		add: (self function: #CallLongMethod:);
		add: (self function: #CallLongMethodV:);
		add: (self function: #CallLongMethodA:obj:methodID:args: returning: 'sqword' withArgs: 'JNIObject JNIMethodID JNIValueArray*');


		"
		jfloat (JNICALL *CallFloatMethod)(JNIEnv *env, jobject obj, jmethodID methodID, ...);
		jfloat (JNICALL *CallFloatMethodV)(JNIEnv *env, jobject obj, jmethodID methodID, va_list args);
		jfloat (JNICALL *CallFloatMethodA)(JNIEnv *env, jobject obj, jmethodID methodID, jvalue *args);
		"
		add: (self function: #CallFloatMethod:);
		add: (self function: #CallFloatMethodV:);
		add: (self function: #CallFloatMethodA:obj:methodID:args: returning: 'float' withArgs: 'JNIObject JNIMethodID JNIValueArray*');


		"
		jdouble (JNICALL *CallDoubleMethod)(JNIEnv *env, jobject obj, jmethodID methodID, ...);
		jdouble (JNICALL *CallDoubleMethodV)(JNIEnv *env, jobject obj, jmethodID methodID, va_list args);
		jdouble (JNICALL *CallDoubleMethodA)(JNIEnv *env, jobject obj, jmethodID methodID, jvalue *args);
		"
		add: (self function: #CallDoubleMethod:);
		add: (self function: #CallDoubleMethodV:);
		add: (self function: #CallDoubleMethodA:obj:methodID:args: returning: 'double' withArgs: 'JNIObject JNIMethodID JNIValueArray*');


		"
		void (JNICALL *CallVoidMethod)(JNIEnv *env, jobject obj, jmethodID methodID, ...);
		void (JNICALL *CallVoidMethodV)(JNIEnv *env, jobject obj, jmethodID methodID, va_list args);
		void (JNICALL *CallVoidMethodA)(JNIEnv *env, jobject obj, jmethodID methodID, jvalue * args);
		"
		add: (self function: #CallVoidMethod:);
		add: (self function: #CallVoidMethodV:);
		add: (self function: #CallVoidMethodA:obj:methodID:args: returning: 'void' withArgs: 'JNIObject JNIMethodID JNIValueArray*');


		"
		jobject (JNICALL *CallNonvirtualObjectMethod)(JNIEnv *env, jobject obj, jclass class, jmethodID methodID, ...);
		jobject (JNICALL *CallNonvirtualObjectMethodV)(JNIEnv *env, jobject obj, jclass class, jmethodID methodID, va_list args);
		jobject (JNICALL *CallNonvirtualObjectMethodA)(JNIEnv *env, jobject obj, jclass class, jmethodID methodID, jvalue * args);
		"
		add: (self function: #CallNonvirtualObjectMethod:);
		add: (self function: #CallNonvirtualObjectMethodV:);
		add: (self function: #CallNonvirtualObjectMethodA:obj:class:methodID:args: returning: 'JNIObject' withArgs: 'JNIObject JNIClass JNIMethodID JNIValueArray*');


		"
		jboolean (JNICALL *CallNonvirtualBooleanMethod)(JNIEnv *env, jobject obj, jclass class, jmethodID methodID, ...);
		jboolean (JNICALL *CallNonvirtualBooleanMethodV)(JNIEnv *env, jobject obj, jclass class, jmethodID methodID, va_list args);
		jboolean (JNICALL *CallNonvirtualBooleanMethodA)(JNIEnv *env, jobject obj, jclass class, jmethodID methodID, jvalue * args);
		"
		add: (self function: #CallNonvirtualBooleanMethod:);
		add: (self function: #CallNonvirtualBooleanMethod:);
		add: (self function: #CallNonvirtualBooleanMethodA:obj:class:methodID:args: returning: 'byte' withArgs: 'JNIObject JNIClass JNIMethodID JNIValueArray*');


		"
		jbyte (JNICALL *CallNonvirtualByteMethod)(JNIEnv *env, jobject obj, jclass class, jmethodID methodID, ...);
		jbyte (JNICALL *CallNonvirtualByteMethodV)(JNIEnv *env, jobject obj, jclass class, jmethodID methodID, va_list args);
		jbyte (JNICALL *CallNonvirtualByteMethodA)(JNIEnv *env, jobject obj, jclass class, jmethodID methodID, jvalue *args);
		"
		add: (self function: #CallNonvirtualByteMethod:);
		add: (self function: #CallNonvirtualByteMethodV:);
		add: (self function: #CallNonvirtualByteMethodA:obj:class:methodID:args: returning: 'sbyte' withArgs: 'JNIObject JNIClass JNIMethodID JNIValueArray*');


		"
		jchar (JNICALL *CallNonvirtualCharMethod)(JNIEnv *env, jobject obj, jclass class, jmethodID methodID, ...);
		jchar (JNICALL *CallNonvirtualCharMethodV)(JNIEnv *env, jobject obj, jclass class, jmethodID methodID, va_list args);
		jchar (JNICALL *CallNonvirtualCharMethodA)(JNIEnv *env, jobject obj, jclass class, jmethodID methodID, jvalue *args);
		"
		add: (self function: #CallNonvirtualCharMethod:);
		add: (self function: #CallNonvirtualCharMethodV:);
		add: (self function: #CallNonvirtualCharMethodA:obj:class:methodID:args: returning: 'word' withArgs: 'JNIObject JNIClass JNIMethodID JNIValueArray*');


		"
		jshort (JNICALL *CallNonvirtualShortMethod)(JNIEnv *env, jobject obj, jclass class, jmethodID methodID, ...);
		jshort (JNICALL *CallNonvirtualShortMethodV)(JNIEnv *env, jobject obj, jclass class, jmethodID methodID, va_list args);
		jshort (JNICALL *CallNonvirtualShortMethodA)(JNIEnv *env, jobject obj, jclass class, jmethodID methodID, jvalue *args);
		"
		add: (self function: #CallNonvirtualShortMethod:);
		add: (self function: #CallNonvirtualShortMethodV:);
		add: (self function: #CallNonvirtualShortMethodA:obj:class:methodID:args: returning: 'sword' withArgs: 'JNIObject JNIClass JNIMethodID JNIValueArray*');


		"
		jint (JNICALL *CallNonvirtualIntMethod)(JNIEnv *env, jobject obj, jclass class, jmethodID methodID, ...);
		jint (JNICALL *CallNonvirtualIntMethodV)(JNIEnv *env, jobject obj, jclass class, jmethodID methodID, va_list args);
		jint (JNICALL *CallNonvirtualIntMethodA)(JNIEnv *env, jobject obj, jclass class, jmethodID methodID, jvalue *args);
		"
		add: (self function: #CallNonvirtualIntMethod:);
		add: (self function: #CallNonvirtualIntMethodV:);
		add: (self function: #CallNonvirtualIntMethodA:obj:class:methodID:args: returning: 'sdword' withArgs: 'JNIObject JNIClass JNIMethodID JNIValueArray*');


		"
		jlong (JNICALL *CallNonvirtualLongMethod)(JNIEnv *env, jobject obj, jclass class, jmethodID methodID, ...);
		jlong (JNICALL *CallNonvirtualLongMethodV)(JNIEnv *env, jobject obj, jclass class, jmethodID methodID, va_list args);
		jlong (JNICALL *CallNonvirtualLongMethodA)(JNIEnv *env, jobject obj, jclass class, jmethodID methodID, jvalue *args);
		"
		add: (self function: #CallNonvirtualLongMethod:);
		add: (self function: #CallNonvirtualLongMethodV:);
		add: (self function: #CallNonvirtualLongMethodA:obj:class:methodID:args: returning: 'sqword' withArgs: 'JNIObject JNIClass JNIMethodID JNIValueArray*');


		"
		jfloat (JNICALL *CallNonvirtualFloatMethod)(JNIEnv *env, jobject obj, jclass class, jmethodID methodID, ...);
		jfloat (JNICALL *CallNonvirtualFloatMethodV)(JNIEnv *env, jobject obj, jclass class, jmethodID methodID, va_list args);
		jfloat (JNICALL *CallNonvirtualFloatMethodA)(JNIEnv *env, jobject obj, jclass class, jmethodID methodID, jvalue *args);
		"
		add: (self function: #CallNonvirtualFloatMethod:);
		add: (self function: #CallNonvirtualFloatMethodV:);
		add: (self function: #CallNonvirtualFloatMethodA:obj:class:methodID:args: returning: 'float' withArgs: 'JNIObject JNIClass JNIMethodID JNIValueArray*');


		"
		jdouble (JNICALL *CallNonvirtualDoubleMethod)(JNIEnv *env, jobject obj, jclass class, jmethodID methodID, ...);
		jdouble (JNICALL *CallNonvirtualDoubleMethodV)(JNIEnv *env, jobject obj, jclass class, jmethodID methodID, va_list args);
		jdouble (JNICALL *CallNonvirtualDoubleMethodA)(JNIEnv *env, jobject obj, jclass class, jmethodID methodID, jvalue *args);
		"
		add: (self function: #CallNonvirtualDoubleMethod:);
		add: (self function: #CallNonvirtualDoubleMethodV:);
		add: (self function: #CallNonvirtualDoubleMethodA:obj:class:methodID:args: returning: 'double' withArgs: 'JNIObject JNIClass JNIMethodID JNIValueArray*');


		"
		void (JNICALL *CallNonvirtualVoidMethod)(JNIEnv *env, jobject obj, jclass class, jmethodID methodID, ...);
		void (JNICALL *CallNonvirtualVoidMethodV)(JNIEnv *env, jobject obj, jclass class, jmethodID methodID, va_list args);
		void (JNICALL *CallNonvirtualVoidMethodA)(JNIEnv *env, jobject obj, jclass class, jmethodID methodID, jvalue * args);
		"
		add: (self function: #CallNonvirtualVoidMethod:);
		add: (self function: #CallNonvirtualVoidMethodV:);
		add: (self function: #CallNonvirtualVoidMethodA:obj:class:methodID:args: returning: 'void' withArgs: 'JNIObject JNIClass JNIMethodID JNIValueArray*');


		"
		jfieldID (JNICALL *GetFieldID)(JNIEnv *env, jclass class, const char *name, const char *sig);
		"
		add: (self function: #GetFieldID:class:name:sig: returning: 'JNIFieldID' withArgs: 'JNIClass char* char*');


		"
		jobject (JNICALL *GetObjectField)(JNIEnv *env, jobject obj, jfieldID fieldID);
		jboolean (JNICALL *GetBooleanField)(JNIEnv *env, jobject obj, jfieldID fieldID);
		jbyte (JNICALL *GetByteField)(JNIEnv *env, jobject obj, jfieldID fieldID);
		jchar (JNICALL *GetCharField)(JNIEnv *env, jobject obj, jfieldID fieldID);
		jshort (JNICALL *GetShortField)(JNIEnv *env, jobject obj, jfieldID fieldID);
		jint (JNICALL *GetIntField)(JNIEnv *env, jobject obj, jfieldID fieldID);
		jlong (JNICALL *GetLongField)(JNIEnv *env, jobject obj, jfieldID fieldID);
		jfloat (JNICALL *GetFloatField)(JNIEnv *env, jobject obj, jfieldID fieldID);
		jdouble (JNICALL *GetDoubleField)(JNIEnv *env, jobject obj, jfieldID fieldID);
		"
		add: (self function: #GetObjectField:obj:fieldID: returning: 'JNIObject' withArgs: 'JNIObject JNIFieldID');
		add: (self function: #GetBooleanField:obj:fieldID: returning: 'byte' withArgs: 'JNIObject JNIFieldID');
		add: (self function: #GetByteField:obj:fieldID: returning: 'sbyte' withArgs: 'JNIObject JNIFieldID');
		add: (self function: #GetCharField:obj:fieldID: returning: 'word' withArgs: 'JNIObject JNIFieldID');
		add: (self function: #GetShortField:obj:fieldID: returning: 'sword' withArgs: 'JNIObject JNIFieldID');
		add: (self function: #GetIntField:obj:fieldID: returning: 'sdword' withArgs: 'JNIObject JNIFieldID');
		add: (self function: #GetLongField:obj:fieldID: returning: 'sqword' withArgs: 'JNIObject JNIFieldID');
		add: (self function: #GetFloatField:obj:fieldID: returning: 'float' withArgs: 'JNIObject JNIFieldID');
		add: (self function: #GetDoubleField:obj:fieldID: returning: 'double' withArgs: 'JNIObject JNIFieldID');


		"
		void (JNICALL *SetObjectField)(JNIEnv *env, jobject obj, jfieldID fieldID, jobject val);
		void (JNICALL *SetBooleanField)(JNIEnv *env, jobject obj, jfieldID fieldID, jboolean val);
		void (JNICALL *SetByteField)(JNIEnv *env, jobject obj, jfieldID fieldID, jbyte val);
		void (JNICALL *SetCharField)(JNIEnv *env, jobject obj, jfieldID fieldID, jchar val);
		void (JNICALL *SetShortField)(JNIEnv *env, jobject obj, jfieldID fieldID, jshort val);
		void (JNICALL *SetIntField)(JNIEnv *env, jobject obj, jfieldID fieldID, jint val);
		void (JNICALL *SetLongField)(JNIEnv *env, jobject obj, jfieldID fieldID, jlong val);
		void (JNICALL *SetFloatField)(JNIEnv *env, jobject obj, jfieldID fieldID, jfloat val);
		void (JNICALL *SetDoubleField)(JNIEnv *env, jobject obj, jfieldID fieldID, jdouble val);
		"
		add: (self function: #SetObjectField:obj:fieldID:val: returning: 'void' withArgs: 'JNIObject JNIFieldID JNIObject');
		add: (self function: #SetBooleanField:obj:fieldID:val: returning: 'void' withArgs: 'JNIObject JNIFieldID byte');
		add: (self function: #SetByteField:obj:fieldID:val: returning: 'void' withArgs: 'JNIObject JNIFieldID sbyte');
		add: (self function: #SetCharField:obj:fieldID:val: returning: 'void' withArgs: 'JNIObject JNIFieldID word');
		add: (self function: #SetShortField:obj:fieldID:val: returning: 'void' withArgs: 'JNIObject JNIFieldID sword');
		add: (self function: #SetIntField:obj:fieldID:val: returning: 'void' withArgs: 'JNIObject JNIFieldID sdword');
		add: (self function: #SetLongField:obj:fieldID:val: returning: 'void' withArgs: 'JNIObject JNIFieldID sqword');
		add: (self function: #SetFloatField:obj:fieldID:val: returning: 'void' withArgs: 'JNIObject JNIFieldID float');
		add: (self function: #SetDoubleField:obj:fieldID:val: returning: 'void' withArgs: 'JNIObject JNIFieldID double');


		"
		jmethodID (JNICALL *GetStaticMethodID)(JNIEnv *env, jclass class, const char *name, const char *sig);
		"
		add: (self function: #GetStaticMethodID:class:name:sig: returning: 'JNIMethodID' withArgs: 'JNIClass char* char*');


		"
		jobject (JNICALL *CallStaticObjectMethod)(JNIEnv *env, jclass class, jmethodID methodID, ...);
		jobject (JNICALL *CallStaticObjectMethodV)(JNIEnv *env, jclass class, jmethodID methodID, va_list args);
		jobject (JNICALL *CallStaticObjectMethodA)(JNIEnv *env, jclass class, jmethodID methodID, jvalue *args);
		"
		add: (self function: #CallStaticObjectMethod:);
		add: (self function: #CallStaticObjectMethodV:);
		add: (self function: #CallStaticObjectMethodA:class:methodID:args: returning: 'JNIObject' withArgs: 'JNIClass JNIMethodID JNIValueArray*');


		"
		jboolean (JNICALL *CallStaticBooleanMethod)(JNIEnv *env, jclass class, jmethodID methodID, ...);
		jboolean (JNICALL *CallStaticBooleanMethodV)(JNIEnv *env, jclass class, jmethodID methodID, va_list args);
		jboolean (JNICALL *CallStaticBooleanMethodA)(JNIEnv *env, jclass class, jmethodID methodID, jvalue *args);
		"
		add: (self function: #CallStaticBooleanMethod:);
		add: (self function: #CallStaticBooleanMethodV:);
		add: (self function: #CallStaticBooleanMethodA:class:methodID:args: returning: 'byte' withArgs: 'JNIClass JNIMethodID JNIValueArray*');


		"
		jbyte (JNICALL *CallStaticByteMethod)(JNIEnv *env, jclass class, jmethodID methodID, ...);
		jbyte (JNICALL *CallStaticByteMethodV)(JNIEnv *env, jclass class, jmethodID methodID, va_list args);
		jbyte (JNICALL *CallStaticByteMethodA)(JNIEnv *env, jclass class, jmethodID methodID, jvalue *args);
		"
		add: (self function: #CallStaticByteMethod:);
		add: (self function: #CallStaticByteMethodV:);
		add: (self function: #CallStaticByteMethodA:class:methodID:args: returning: 'sbyte' withArgs: 'JNIClass JNIMethodID JNIValueArray*');


		"
		jchar (JNICALL *CallStaticCharMethod)(JNIEnv *env, jclass class, jmethodID methodID, ...);
		jchar (JNICALL *CallStaticCharMethodV)(JNIEnv *env, jclass class, jmethodID methodID, va_list args);
		jchar (JNICALL *CallStaticCharMethodA)(JNIEnv *env, jclass class, jmethodID methodID, jvalue *args);
		"
		add: (self function: #CallStaticCharMethod:);
		add: (self function: #CallStaticCharMethodV:);
		add: (self function: #CallStaticCharMethodA:class:methodID:args: returning: 'word' withArgs: 'JNIClass JNIMethodID JNIValueArray*');


		"
		jshort (JNICALL *CallStaticShortMethod)(JNIEnv *env, jclass class, jmethodID methodID, ...);
		jshort (JNICALL *CallStaticShortMethodV)(JNIEnv *env, jclass class, jmethodID methodID, va_list args);
		jshort (JNICALL *CallStaticShortMethodA)(JNIEnv *env, jclass class, jmethodID methodID, jvalue *args);
		"
		add: (self function: #CallStaticShortMethod:);
		add: (self function: #CallStaticShortMethodV:);
		add: (self function: #CallStaticShortMethodA:class:methodID:args: returning: 'sword' withArgs: 'JNIClass JNIMethodID JNIValueArray*');


		"
		jint (JNICALL *CallStaticIntMethod)(JNIEnv *env, jclass class, jmethodID methodID, ...);
		jint (JNICALL *CallStaticIntMethodV)(JNIEnv *env, jclass class, jmethodID methodID, va_list args);
		jint (JNICALL *CallStaticIntMethodA)(JNIEnv *env, jclass class, jmethodID methodID, jvalue *args);
		"
		add: (self function: #CallStaticIntMethod:);
		add: (self function: #CallStaticIntMethodV:);
		add: (self function: #CallStaticIntMethodA:class:methodID:args: returning: 'sdword' withArgs: 'JNIClass JNIMethodID JNIValueArray*');


		"
		jlong (JNICALL *CallStaticLongMethod)(JNIEnv *env, jclass class, jmethodID methodID, ...);
		jlong (JNICALL *CallStaticLongMethodV)(JNIEnv *env, jclass class, jmethodID methodID, va_list args);
		jlong (JNICALL *CallStaticLongMethodA)(JNIEnv *env, jclass class, jmethodID methodID, jvalue *args);
		"
		add: (self function: #CallStaticLongMethod:);
		add: (self function: #CallStaticLongMethodV:);
		add: (self function: #CallStaticLongMethodA:class:methodID:args: returning: 'sqword' withArgs: 'JNIClass JNIMethodID JNIValueArray*');


		"
		jfloat (JNICALL *CallStaticFloatMethod)(JNIEnv *env, jclass class, jmethodID methodID, ...);
		jfloat (JNICALL *CallStaticFloatMethodV)(JNIEnv *env, jclass class, jmethodID methodID, va_list args);
		jfloat (JNICALL *CallStaticFloatMethodA)(JNIEnv *env, jclass class, jmethodID methodID, jvalue *args);
		"
		add: (self function: #CallStaticFloatMethod:);
		add: (self function: #CallStaticFloatMethodV:);
		add: (self function: #CallStaticFloatMethodA:class:methodID:args: returning: 'float' withArgs: 'JNIClass JNIMethodID JNIValueArray*');


		"
		jdouble (JNICALL *CallStaticDoubleMethod)(JNIEnv *env, jclass class, jmethodID methodID, ...);
		jdouble (JNICALL *CallStaticDoubleMethodV)(JNIEnv *env, jclass class, jmethodID methodID, va_list args);
		jdouble (JNICALL *CallStaticDoubleMethodA)(JNIEnv *env, jclass class, jmethodID methodID, jvalue *args);
		"
		add: (self function: #CallStaticDoubleMethod:);
		add: (self function: #CallStaticDoubleMethodV:);
		add: (self function: #CallStaticDoubleMethodA:class:methodID:args: returning: 'double' withArgs: 'JNIClass JNIMethodID JNIValueArray*');


		"
		void (JNICALL *CallStaticVoidMethod)(JNIEnv *env, jclass class, jmethodID methodID, ...);
		void (JNICALL *CallStaticVoidMethodV)(JNIEnv *env, jclass class, jmethodID methodID, va_list args);
		void (JNICALL *CallStaticVoidMethodA)(JNIEnv *env, jclass class, jmethodID methodID, jvalue * args);
		"
		add: (self function: #CallStaticVoidMethod:);
		add: (self function: #CallStaticVoidMethodV:);
		add: (self function: #CallStaticVoidMethodA:class:methodID:args: returning: 'void' withArgs: 'JNIClass JNIMethodID JNIValueArray*');


		"
		jfieldID (JNICALL *GetStaticFieldID)(JNIEnv *env, jclass class, const char *name, const char *sig);
		jobject (JNICALL *GetStaticObjectField)(JNIEnv *env, jclass class, jfieldID fieldID);
		jboolean (JNICALL *GetStaticBooleanField)(JNIEnv *env, jclass class, jfieldID fieldID);
		jbyte (JNICALL *GetStaticByteField)(JNIEnv *env, jclass class, jfieldID fieldID);
		jchar (JNICALL *GetStaticCharField)(JNIEnv *env, jclass class, jfieldID fieldID);
		jshort (JNICALL *GetStaticShortField)(JNIEnv *env, jclass class, jfieldID fieldID);
		jint (JNICALL *GetStaticIntField)(JNIEnv *env, jclass class, jfieldID fieldID);
		jlong (JNICALL *GetStaticLongField)(JNIEnv *env, jclass class, jfieldID fieldID);
		jfloat (JNICALL *GetStaticFloatField)(JNIEnv *env, jclass class, jfieldID fieldID);
		jdouble (JNICALL *GetStaticDoubleField)(JNIEnv *env, jclass class, jfieldID fieldID);
		"
		add: (self function: #GetStaticFieldID:class:name:sig: returning: 'JNIFieldID' withArgs: 'JNIClass char* char*');
		add: (self function: #GetStaticObjectField:class:fieldID: returning: 'JNIObject' withArgs: 'JNIClass JNIFieldID');
		add: (self function: #GetStaticBooleanField:class:fieldID: returning: 'byte' withArgs: 'JNIClass JNIFieldID');
		add: (self function: #GetStaticByteField:class:fieldID: returning: 'sbyte' withArgs: 'JNIClass JNIFieldID');
		add: (self function: #GetStaticCharField:class:fieldID: returning: 'word' withArgs: 'JNIClass JNIFieldID');
		add: (self function: #GetStaticShortField:class:fieldID: returning: 'sword' withArgs: 'JNIClass JNIFieldID');
		add: (self function: #GetStaticIntField:class:fieldID: returning: 'sdword' withArgs: 'JNIClass JNIFieldID');
		add: (self function: #GetStaticLongField:class:fieldID: returning: 'sqword' withArgs: 'JNIClass JNIFieldID');
		add: (self function: #GetStaticFloatField:class:fieldID: returning: 'float' withArgs: 'JNIClass JNIFieldID');
		add: (self function: #GetStaticDoubleField:class:fieldID: returning: 'double' withArgs: 'JNIClass JNIFieldID');


		" 
		void (JNICALL *SetStaticObjectField)(JNIEnv *env, jclass class, jfieldID fieldID, jobject val);
		void (JNICALL *SetStaticBooleanField)(JNIEnv *env, jclass class, jfieldID fieldID, jboolean val);
		void (JNICALL *SetStaticByteField)(JNIEnv *env, jclass class, jfieldID fieldID, jbyte val);
		void (JNICALL *SetStaticCharField)(JNIEnv *env, jclass class, jfieldID fieldID, jchar val);
		void (JNICALL *SetStaticShortField)(JNIEnv *env, jclass class, jfieldID fieldID, jshort val);
		void (JNICALL *SetStaticIntField)(JNIEnv *env, jclass class, jfieldID fieldID, jint val);
		void (JNICALL *SetStaticLongField)(JNIEnv *env, jclass class, jfieldID fieldID, jlong val);
		void (JNICALL *SetStaticFloatField)(JNIEnv *env, jclass class, jfieldID fieldID, jfloat val);
		void (JNICALL *SetStaticDoubleField)(JNIEnv *env, jclass class, jfieldID fieldID, jdouble val);
		"
		add: (self function: #SetStaticObjectField:class:fieldID:val: returning: 'void' withArgs: 'JNIClass JNIFieldID JNIObject');
		add: (self function: #SetStaticBooleanField:class:fieldID:val: returning: 'void' withArgs: 'JNIClass JNIFieldID byte');
		add: (self function: #SetStaticByteField:class:fieldID:val: returning: 'void' withArgs: 'JNIClass JNIFieldID sbyte');
		add: (self function: #SetStaticCharField:class:fieldID:val: returning: 'void' withArgs: 'JNIClass JNIFieldID word');
		add: (self function: #SetStaticShortField:class:fieldID:val: returning: 'void' withArgs: 'JNIClass JNIFieldID sword');
		add: (self function: #SetStaticIntField:class:fieldID:val: returning: 'void' withArgs: 'JNIClass JNIFieldID sdword');
		add: (self function: #SetStaticLongField:class:fieldID:val: returning: 'void' withArgs: 'JNIClass JNIFieldID sqword');
		add: (self function: #SetStaticFloatField:class:fieldID:val: returning: 'void' withArgs: 'JNIClass JNIFieldID float');
		add: (self function: #SetStaticDoubleField:class:fieldID:val: returning: 'void' withArgs: 'JNIClass JNIFieldID double');


		"
		jstring (JNICALL *NewString)(JNIEnv *env, const jchar *unicode, jsize len);
		jsize (JNICALL *GetStringLength)(JNIEnv *env, jstring str);
		const jchar *(JNICALL *GetStringChars)(JNIEnv *env, jstring str, jboolean *isCopy);
		void (JNICALL *ReleaseStringChars)(JNIEnv *env, jstring str, const jchar *chars);
		"
		add: (self function: #NewString:unicode:len: returning: 'JNIString' withArgs: 'word* sdword');
		add: (self function: #GetStringLength:str: returning: 'sdword' withArgs: 'JNIString');
		add: (self function: #GetStringChars:str:isCopy: returning: 'void*' withArgs: 'JNIString byte*');
		add: (self function: #ReleaseStringChars:str:chars: returning: 'void' withArgs: 'JNIString word*');


		"
		jstring (JNICALL *NewStringUTF)(JNIEnv *env, const char *utf);
		jsize (JNICALL *GetStringUTFLength)(JNIEnv *env, jstring str);
		const char* (JNICALL *GetStringUTFChars)(JNIEnv *env, jstring str, jboolean *isCopy);
		void (JNICALL *ReleaseStringUTFChars)(JNIEnv *env, jstring str, const char* chars);
		"
		add: (self function: #NewStringUTF:utf: returning: 'JNIString' withArgs: 'byte*');
		add: (self function: #GetStringUTFLength:str: returning: 'sdword' withArgs: 'JNIString');
		add: (self function: #GetStringUTFChars:str:isCopy: returning: 'void*' withArgs: 'JNIString byte*');
		add: (self function: #ReleaseStringUTFChars:str:chars: returning: 'void' withArgs: 'JNIString byte*');


		"
		jsize (JNICALL *GetArrayLength)(JNIEnv *env, jarray array);
		"
		add: (self function: #GetArrayLength:array: returning: 'sdword' withArgs: 'JNIArray');


		"
		jobjectArray (JNICALL *NewObjectArray)(JNIEnv *env, jsize len, jclass class, jobject init);
		jobject (JNICALL *GetObjectArrayElement)(JNIEnv *env, jobjectArray array, jsize index);
		void (JNICALL *SetObjectArrayElement)(JNIEnv *env, jobjectArray array, jsize index, jobject val);
		"
		add: (self function: #NewObjectArray:len:class:init: returning: 'JNIObjectArray' withArgs: 'sdword JNIClass JNIObject');
		add: (self function: #GetObjectArrayElement:array:index: returning: 'JNIObject' withArgs: 'JNIObjectArray sdword');
		add: (self function: #SetObjectArrayElement:array:index:val: returning: 'void' withArgs: 'JNIObjectArray sdword JNIObject');


		"
		jbooleanArray (JNICALL *NewBooleanArray)(JNIEnv *env, jsize len);
		jbyteArray (JNICALL *NewByteArray)(JNIEnv *env, jsize len);
		jcharArray (JNICALL *NewCharArray)(JNIEnv *env, jsize len);
		jshortArray (JNICALL *NewShortArray)(JNIEnv *env, jsize len);
		jintArray (JNICALL *NewIntArray)(JNIEnv *env, jsize len);
		jlongArray (JNICALL *NewLongArray)(JNIEnv *env, jsize len);
		jfloatArray (JNICALL *NewFloatArray)(JNIEnv *env, jsize len);
		jdoubleArray (JNICALL *NewDoubleArray)(JNIEnv *env, jsize len);
		"
		add: (self function: #NewBooleanArray:len: returning: 'JNIBooleanArray' withArgs: 'sdword');
		add: (self function: #NewByteArray:len: returning: 'JNIByteArray' withArgs: 'sdword');
		add: (self function: #NewCharArray:len: returning: 'JNICharArray' withArgs: 'sdword');
		add: (self function: #NewShortArray:len: returning: 'JNIShortArray' withArgs: 'sdword');
		add: (self function: #NewIntArray:len: returning: 'JNIIntArray' withArgs: 'sdword');
		add: (self function: #NewLongArray:len: returning: 'JNILongArray' withArgs: 'sdword');
		add: (self function: #NewFloatArray:len: returning: 'JNIFloatArray' withArgs: 'sdword');
		add: (self function: #NewDoubleArray:len: returning: 'JNIDoubleArray' withArgs: 'sdword');


		"
		jboolean * (JNICALL *GetBooleanArrayElements)(JNIEnv *env, jbooleanArray array, jboolean *isCopy);
		jbyte * (JNICALL *GetByteArrayElements)(JNIEnv *env, jbyteArray array, jboolean *isCopy);
		jchar * (JNICALL *GetCharArrayElements)(JNIEnv *env, jcharArray array, jboolean *isCopy);
		jshort * (JNICALL *GetShortArrayElements)(JNIEnv *env, jshortArray array, jboolean *isCopy);
		jint * (JNICALL *GetIntArrayElements)(JNIEnv *env, jintArray array, jboolean *isCopy);
		jlong * (JNICALL *GetLongArrayElements)(JNIEnv *env, jlongArray array, jboolean *isCopy);
		jfloat * (JNICALL *GetFloatArrayElements)(JNIEnv *env, jfloatArray array, jboolean *isCopy);
		jdouble * (JNICALL *GetDoubleArrayElements)(JNIEnv *env, jdoubleArray array, jboolean *isCopy);
		"
		add: (self function: #GetBooleanArrayElements:array:isCopy: returning: 'void*' withArgs: 'JNIBooleanArray byte*');
		add: (self function: #GetByteArrayElements:array:isCopy: returning: 'void*' withArgs: 'JNIByteArray byte*');
		add: (self function: #GetCharArrayElements:array:isCopy: returning: 'void*' withArgs: 'JNICharArray byte*');
		add: (self function: #GetShortArrayElements:array:isCopy: returning: 'void*' withArgs: 'JNIShortArray byte*');
		add: (self function: #GetIntArrayElements:array:isCopy: returning: 'void*' withArgs: 'JNIIntArray byte*');
		add: (self function: #GetLongArrayElements:array:isCopy: returning: 'void*' withArgs: 'JNILongArray byte*');
		add: (self function: #GetFloatArrayElements:array:isCopy: returning: 'void*' withArgs: 'JNIFloatArray byte*');
		add: (self function: #GetDoubleArrayElements:array:isCopy: returning: 'void*' withArgs: 'JNIDoubleArray byte*');


		"
		void (JNICALL *ReleaseBooleanArrayElements)(JNIEnv *env, jbooleanArray array, jboolean *elems, jint mode);
		void (JNICALL *ReleaseByteArrayElements)(JNIEnv *env, jbyteArray array, jbyte *elems, jint mode);
		void (JNICALL *ReleaseCharArrayElements)(JNIEnv *env, jcharArray array, jchar *elems, jint mode);
		void (JNICALL *ReleaseShortArrayElements)(JNIEnv *env, jshortArray array, jshort *elems, jint mode);
		void (JNICALL *ReleaseIntArrayElements)(JNIEnv *env, jintArray array, jint *elems, jint mode);
		void (JNICALL *ReleaseLongArrayElements)(JNIEnv *env, jlongArray array, jlong *elems, jint mode);
		void (JNICALL *ReleaseFloatArrayElements)(JNIEnv *env, jfloatArray array, jfloat *elems, jint mode);
		void (JNICALL *ReleaseDoubleArrayElements)(JNIEnv *env, jdoubleArray array, jdouble *elems, jint mode);
		"
		add: (self function: #ReleaseBooleanArrayElements:array:elems:mode: returning: 'void' withArgs: 'JNIBooleanArray void* sdword');
		add: (self function: #ReleaseByteArrayElements:array:elems:mode: returning: 'void' withArgs: 'JNIByteArray void* sdword');
		add: (self function: #ReleaseCharArrayElements:array:elems:mode: returning: 'void' withArgs: 'JNICharArray void* sdword');
		add: (self function: #ReleaseShortArrayElements:array:elems:mode: returning: 'void' withArgs: 'JNIShortArray void* sdword');
		add: (self function: #ReleaseIntArrayElements:array:elems:mode: returning: 'void' withArgs: 'JNIIntArray void* sdword');
		add: (self function: #ReleaseLongArrayElements:array:elems:mode: returning: 'void' withArgs: 'JNILongArray void* sdword');
		add: (self function: #ReleaseFloatArrayElements:array:elems:mode: returning: 'void' withArgs: 'JNIFloatArray void* sdword');
		add: (self function: #ReleaseDoubleArrayElements:array:elems:mode: returning: 'void' withArgs: 'JNIDoubleArray void* sdword');


		"
		void (JNICALL *GetBooleanArrayRegion)(JNIEnv *env, jbooleanArray array, jsize start, jsize len, jboolean *buf);
		void (JNICALL *GetByteArrayRegion)(JNIEnv *env, jbyteArray array, jsize start, jsize len, jbyte *buf);
		void (JNICALL *GetCharArrayRegion)(JNIEnv *env, jcharArray array, jsize start, jsize len, jchar *buf);
		void (JNICALL *GetShortArrayRegion)(JNIEnv *env, jshortArray array, jsize start, jsize len, jshort *buf);
		void (JNICALL *GetIntArrayRegion)(JNIEnv *env, jintArray array, jsize start, jsize len, jint *buf);
		void (JNICALL *GetLongArrayRegion)(JNIEnv *env, jlongArray array, jsize start, jsize len, jlong *buf);
		void (JNICALL *GetFloatArrayRegion)(JNIEnv *env, jfloatArray array, jsize start, jsize len, jfloat *buf);
		void (JNICALL *GetDoubleArrayRegion)(JNIEnv *env, jdoubleArray array, jsize start, jsize len, jdouble *buf);
		"
		add: (self function: #GetBooleanArrayRegion:array:start:len:buf: returning: 'void' withArgs: 'JNIBooleanArray sdword sdword byte*');
		add: (self function: #GetByteArrayRegion:array:start:len:buf: returning: 'void' withArgs: 'JNIByteArray sdword sdword sbyte*');
		add: (self function: #GetCharArrayRegion:array:start:len:buf: returning: 'void' withArgs: 'JNICharArray sdword sdword word*');
		add: (self function: #GetShortArrayRegion:array:start:len:buf: returning: 'void' withArgs: 'JNIShortArray sdword sdword sword*');
		add: (self function: #GetIntArrayRegion:array:start:len:buf: returning: 'void' withArgs: 'JNIIntArray sdword sdword sdword*');
		add: (self function: #GetLongArrayRegion:array:start:len:buf: returning: 'void' withArgs: 'JNILongArray sdword sdword sqword*');
		add: (self function: #GetFloatArrayRegion:array:start:len:buf: returning: 'void' withArgs: 'JNIFloatArray sdword sdword float*');
		add: (self function: #GetDoubleArrayRegion:array:start:len:buf: returning: 'void' withArgs: 'JNIDoubleArray sdword sdword double*');


		"
		void (JNICALL *SetBooleanArrayRegion)(JNIEnv *env, jbooleanArray array, jsize start, jsize len, jboolean *buf);
		void (JNICALL *SetByteArrayRegion)(JNIEnv *env, jbyteArray array, jsize start, jsize len, jbyte *buf);
		void (JNICALL *SetCharArrayRegion)(JNIEnv *env, jcharArray array, jsize start, jsize len, jchar *buf);
		void (JNICALL *SetShortArrayRegion)(JNIEnv *env, jshortArray array, jsize start, jsize len, jshort *buf);
		void (JNICALL *SetIntArrayRegion)(JNIEnv *env, jintArray array, jsize start, jsize len, jint *buf);
		void (JNICALL *SetLongArrayRegion)(JNIEnv *env, jlongArray array, jsize start, jsize len, jlong *buf);
		void (JNICALL *SetFloatArrayRegion)(JNIEnv *env, jfloatArray array, jsize start, jsize len, jfloat *buf);
		void (JNICALL *SetDoubleArrayRegion)(JNIEnv *env, jdoubleArray array, jsize start, jsize len, jdouble *buf);
		"
		add: (self function: #SetBooleanArrayRegion:array:start:len:buf: returning: 'void' withArgs: 'JNIBooleanArray sdword sdword byte*');
		add: (self function: #SetByteArrayRegion:array:start:len:buf: returning: 'void' withArgs: 'JNIByteArray sdword sdword sbyte*');
		add: (self function: #SetCharArrayRegion:array:start:len:buf: returning: 'void' withArgs: 'JNICharArray sdword sdword word*');
		add: (self function: #SetShortArrayRegion:array:start:len:buf: returning: 'void' withArgs: 'JNIShortArray sdword sdword sword*');
		add: (self function: #SetIntArrayRegion:array:start:len:buf: returning: 'void' withArgs: 'JNIIntArray sdword sdword sdword*');
		add: (self function: #SetLongArrayRegion:array:start:len:buf: returning: 'void' withArgs: 'JNILongArray sdword sdword sqword*');
		add: (self function: #SetFloatArrayRegion:array:start:len:buf: returning: 'void' withArgs: 'JNIFloatArray sdword sdword float*');
		add: (self function: #SetDoubleArrayRegion:array:start:len:buf: returning: 'void' withArgs: 'JNIDoubleArray sdword sdword double*');


		"
		jint (JNICALL *RegisterNatives)(JNIEnv *env, jclass class, const JNINativeMethod *methods, jint nMethods);
		jint (JNICALL *UnregisterNatives)(JNIEnv *env, jclass class);
		"
		add: (self function: #RegisterNatives:class:methods:nMethods: returning: 'sdword' withArgs: 'JNIClass JNINativeMethod* sdword');
		add: (self function: #UnregisterNatives:class: returning: 'sdword' withArgs: 'JNIClass');


		"
		jint (JNICALL *MonitorEnter)(JNIEnv *env, jobject obj);
		jint (JNICALL *MonitorExit)(JNIEnv *env, jobject obj);
		"
		add: (self function: #MonitorEnter:obj: returning: 'sdword' withArgs: 'JNIObject');
		add: (self function: #MonitorExit:obj: returning: 'sdword' withArgs: 'JNIObject');


		"
		jint (JNICALL *GetJavaVM)(JNIEnv *env, JavaVM **pvm);
		"
		add: (self function: #GetJavaVM:pvm: returning: 'sdword' withArgs: 'JavaVM**');


		"
		void (JNICALL *GetStringRegion)(JNIEnv *env, jstring str, jsize start, jsize len, jchar *buf);
		void (JNICALL *GetStringUTFRegion)(JNIEnv *env, jstring str, jsize start, jsize len, char *buf);
		"
		add: (self function: #GetStringRegion:str:start:len:buf: returning: 'void' withArgs: 'JNIString sdword sdword word*');
		add: (self function: #GetStringUTFRegion:str:start:len:buf: returning: 'void' withArgs: 'JNIString sdword sdword byte*');	


		"
		void * (JNICALL *GetPrimitiveArrayCritical)(JNIEnv *env, jarray array, jboolean *isCopy);
		void (JNICALL *ReleasePrimitiveArrayCritical)(JNIEnv *env, jarray array, void *carray, jint mode);
		"
		add: (self function: #GetPrimitiveArrayCritical:array:isCopy: returning: 'void*' withArgs: 'JNIArray byte*');
		add: (self function: #ReleasePrimitiveArrayCritical:array:carray:mode: returning: 'void' withArgs: 'JNIArray void* sdword');


		"
		const jchar * (JNICALL *GetStringCritical)(JNIEnv *env, jstring str, jboolean *isCopy);
		void (JNICALL *ReleaseStringCritical)(JNIEnv *env, jstring str, const jchar *cstring);
		"
		add: (self function: #GetStringCritical:str:isCopy: returning: 'void*' withArgs: 'JNIString byte*');
		add: (self function: #ReleaseStringCritical:str:cstring: returning: 'void' withArgs: 'JNIString word*');


		"
		jweak (JNICALL *NewWeakGlobalRef)(JNIEnv *env, jobject obj);
		void (JNICALL *DeleteWeakGlobalRef)(JNIEnv *env, jweak obj);
		"
		add: (self function: #NewWeakGlobalRef:obj: returning: 'JNIWeakGlobalRef' withArgs: 'JNIObject');
		add: (self function: #DeleteWeakGlobalRef:ref: returning: 'void' withArgs: 'JNIWeakGlobalRef');


		"
		jboolean (JNICALL *ExceptionCheck)(JNIEnv *env);

		"
		add: (self function: #ExceptionCheck: returning: 'byte');


		"============== added in J2SDK1.4 =============="

		"
		jobject (JNICALL *NewDirectByteBuffer)(JNIEnv* env, void* address, jlong capacity);
		void* (JNICALL *GetDirectBufferAddress)(JNIEnv* env, jobject buf);
		jlong (JNICALL *GetDirectBufferCapacity) (JNIEnv* env, jobject buf);
		"
		add: (self function: #NewDirectByteBuffer:address:capacity: returning: 'JNIObject' withArgs: 'lpvoid sqword');		
		add: (self function: #GetDirectBufferAddress:buf: returning: 'lpvoid' withArgs: 'JNIObject');			
		add: (self function: #GetDirectBufferCapacity:buf: returning: 'sqword' withArgs: 'JNIObject');			


		yourself.! !
!JNINativeInterface class categoriesFor: #function:!helpers!private! !
!JNINativeInterface class categoriesFor: #function:returning:!helpers!private! !
!JNINativeInterface class categoriesFor: #function:returning:withArgs:!helpers!private! !
!JNINativeInterface class categoriesFor: #functionPointerDefinitions!constants!public! !

JNIVTableClient guid: (GUID fromString: '{C2E0794B-1FED-4404-8D8A-DDA4BFFCE6F6}')!
JNIVTableClient comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

See the comment for JNIVTableStructure for a more detailed explanation of what''s going on here.

Basically we represent C++/COM like objects where our first instance variable is a pointer to a vtable.  Hence we can define methods on ourself which invoke the function pointers stored in the corresponding vtable.'!
!JNIVTableClient categoriesForClass!Unclassified! !
!JNIVTableClient methodsFor!

printOn: aStream
	"write a developer-friendly representation of ourselves to aStream"

	"we don't want the complexity of the ExternalStructure definition"
	^ self basicPrintOn: aStream.! !
!JNIVTableClient categoriesFor: #printOn:!printing!public! !

!JNIVTableClient class methodsFor!

defineFields

	"we have a pointer-to-vtable at offset 0"
	self defineField: #vtable type: (PointerField type: self vtableClass) beReadOnly.

!

definesNewFields
	"private -- overridden to take account of our definition of defineFields"

	"NB: subclasses that wish to extend #defineFields will have to override this too"
	^ self class includesSelector: #vtableClass.
!

defineVirtualMethodsInCategories: aCollectionOfCategories
	"private -- helps write the code"

	| vtabOffset |

	vtabOffset := 1.		"the VM uses 1-based offsets"
	self vtableClass functionPointerDefinitions do:
		[:each || selector descriptor str |
		selector := each first.
		descriptor := each second.
		descriptor notNil ifTrue:
			[str := self virtualMethodSourceFor: selector descriptor: descriptor offset: vtabOffset.
			self compile: str categories: aCollectionOfCategories].
		vtabOffset := vtabOffset + 1].	
!

fix: aString
	"private -- the ExternalDescriptor is apt to get things wrong"

	^ Fixes at: aString ifAbsent: [aString].!

formatDescriptor: anExternalDescriptor offset: anOffset
	"private -- make a descriptor look like a primitive method spec"

	| str args |

	str := String writeStream: 50.

	str
		display: '<virtual ';
		display: anExternalDescriptor callingConvention;
		space;
		display: (self fix: anExternalDescriptor returnType);
		space;
		display: anOffset.

	args := anExternalDescriptor argumentTypes.
	args from: 2 to: args size do: [:each | str space; display: (self fix: each)].

	str display: '>'.

	^ str contents.
!

generateVirtualMethods
	"private -- helps write the code"

	| categories |

	categories := Array
				with: (MethodCategory name: 'raw JNI functions')
				with: (MethodCategory name: '**auto generated**')
				with: MethodCategory private.

	self defineVirtualMethodsInCategories: categories.
!

initialize
	"private -- class initialisation.

		self initialize.
	"

	Fixes := LookupTable new.

	#(
		'void'		'lpvoid'		'lppvoid'
		'bool'
		'char'		'byte'		'sbyte'
		'word'		'sword'
		'dword'	'qword'
		'sqword'	'sdword'
		'float'		'double'
		'lpstr'		'lpwstr'
	) do:
		[:each || type oddity |
		type := ExternalDescriptor typeFromName: each.
		oddity := ExternalDescriptor nameOf: type type: nil.
		each = oddity ifFalse: [Fixes at: oddity put: each]].!

virtualMethodPatternFor: aSymbol
	"private -- answer a method header suitable for a virtual method implementing aSymbol"

	| str keywords |

	"NB: we 'know' that we have at least one parameter and that the first must always be the receiver"

	keywords := aSymbol keywords.
	str := String writeStream: 100.

	str display: (keywords first copyWithout: $:).

	2 to: keywords size do:
		[:i |
		i = 2
			ifTrue: [str display: '_']
			ifFalse: [str space].
		str
			display: (keywords at: i);
			space; display: 'argument';
			display: i-1].

	^ str contents.
!

virtualMethodSourceFor: aSymbol descriptor: anExternalDescriptor offset: anOffset
	"private -- answer the source for a method which treats the reviever as an instance
	of a C++ (or COM) class and finds the actual method code via an indirection at
	the given offset in a vtable pointed to by the first 4 bytes of the receiver"

	^ (String writeStream: 200)
		display: (self virtualMethodPatternFor: aSymbol);
		crtab;
		display: '"mechanically generated method to go via our vtable to invoke ';
		print: aSymbol;
		display: '.';
		crtab;
		display: 'See: ';
		display: self vtableClass name;
		display: ' class>>functionPointerDefinitions';
		display: '"';
		cr; crtab;
		display: (self formatDescriptor: anExternalDescriptor offset: anOffset);
		crtab;
		display: '^ self invalidCall.';
		cr;
		contents.
! !
!JNIVTableClient class categoriesFor: #defineFields!constants!public! !
!JNIVTableClient class categoriesFor: #definesNewFields!private!testing! !
!JNIVTableClient class categoriesFor: #defineVirtualMethodsInCategories:!generating methods!private! !
!JNIVTableClient class categoriesFor: #fix:!generating methods!private! !
!JNIVTableClient class categoriesFor: #formatDescriptor:offset:!generating methods!private! !
!JNIVTableClient class categoriesFor: #generateVirtualMethods!generating methods!private! !
!JNIVTableClient class categoriesFor: #initialize!initializing!private! !
!JNIVTableClient class categoriesFor: #virtualMethodPatternFor:!generating methods!private! !
!JNIVTableClient class categoriesFor: #virtualMethodSourceFor:descriptor:offset:!generating methods!private! !

JavaVM guid: (GUID fromString: '{EB4AA509-F472-4356-A020-5FEB8C2CDD25}')!
JavaVM comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Wrapper for JNI''s JavaVM type.

Note: the name of this class is derived from the JNI documentation.  It is bloody stupid.  Instances are *not* JVM''s (neither here nor in JNI), they are simply structures that allow access to JNI functionality on a non-per-thread basis (mostly for administering the relationship between the JVM and the various client threads).

-------------
	self instanceClass compileDefinition.
	self instanceClass generateVirtualMethods.
-------------
'!
!JavaVM categoriesForClass!Unclassified! !
!JavaVM methodsFor!

AttachCurrentThread_penv: argument1 threadArgs: argument2
	"mechanically generated method to go via our vtable to invoke #AttachCurrentThread:penv:threadArgs:.
	See: JNIInvokeInterface class>>functionPointerDefinitions"

	<virtual stdcall: sdword 5 lppvoid lpvoid>
	^ self invalidCall.
!

AttachCurrentThreadAsDaemon_penv: argument1 threadArgs: argument2
	"mechanically generated method to go via our vtable to invoke #AttachCurrentThreadAsDaemon:penv:threadArgs:.
	See: JNIInvokeInterface class>>functionPointerDefinitions"

	<virtual stdcall: sdword 8 lppvoid lpvoid>
	^ self invalidCall.
!

checkReturnCode: aNumber
	"private -- check the return code from a JNI call, and -- if it is not OK -- throw a corresponding JNIError"

	aNumber = JNI_OK ifFalse: [self signalError: aNumber].!

destroy
	"invoke the underlying destroy operation, checking for errors"

	| return |

	return := self DestroyJavaVM.
	self checkReturnCode: return.
!

DestroyJavaVM
	"mechanically generated method to go via our vtable to invoke #DestroyJavaVM:.
	See: JNIInvokeInterface class>>functionPointerDefinitions"

	<virtual stdcall: sdword 4>
	^ self invalidCall.
!

DetachCurrentThread
	"mechanically generated method to go via our vtable to invoke #DetachCurrentThread:.
	See: JNIInvokeInterface class>>functionPointerDefinitions"

	<virtual stdcall: sdword 6>
	^ self invalidCall.
!

errorClass
	"private -- answer the class of Exception to use to signal JNI errors"

	^ JNIError.!

GetEnv_penv: argument1 version: argument2
	"mechanically generated method to go via our vtable to invoke #GetEnv:penv:version:.
	See: JNIInvokeInterface class>>functionPointerDefinitions"

	<virtual stdcall: sdword 7 lppvoid sdword>
	^ self invalidCall.
!

newJNIEnvVersion: anInteger
	"answer a new JNIEnv instance with the given version if that is supported or nil
	if not"

	| jniEnv return |

	jniEnv := JNIEnv newWithJavaVM: self.
	return := self GetEnv_penv: jniEnv version: anInteger.

	"check for version not supported"
	return = JNI_EVERSION ifTrue: [^ nil].	

	self checkReturnCode: return.

	^ jniEnv.!

signalError: aNumber
	"private -- throw an exception corresponding to the given JNI error code"

	self errorClass
		signal: (JNILibrary lookupErrorCode: aNumber)
		with: aNumber.!

vtable
	"Answer the receiver's vtable field as a Smalltalk object."

	^JNIInvokeInterface fromAddress: (bytes sdwordAtOffset: 0).! !
!JavaVM categoriesFor: #AttachCurrentThread_penv:threadArgs:!**auto generated**!private!raw JNI functions! !
!JavaVM categoriesFor: #AttachCurrentThreadAsDaemon_penv:threadArgs:!**auto generated**!private!raw JNI functions! !
!JavaVM categoriesFor: #checkReturnCode:!helpers!private! !
!JavaVM categoriesFor: #destroy!operations!public! !
!JavaVM categoriesFor: #DestroyJavaVM!**auto generated**!private!raw JNI functions! !
!JavaVM categoriesFor: #DetachCurrentThread!**auto generated**!private!raw JNI functions! !
!JavaVM categoriesFor: #errorClass!constants!exceptions!private! !
!JavaVM categoriesFor: #GetEnv_penv:version:!**auto generated**!private!raw JNI functions! !
!JavaVM categoriesFor: #newJNIEnvVersion:!operations!public! !
!JavaVM categoriesFor: #signalError:!exceptions!private! !
!JavaVM categoriesFor: #vtable!**compiled accessors**!public! !

!JavaVM class methodsFor!

vtableClass
	"private -- answer the class which defines the layout of our vtable"

	^ JNIInvokeInterface.! !
!JavaVM class categoriesFor: #vtableClass!constants!private! !

JNIEnv guid: (GUID fromString: '{AC84327F-7CDD-4C3A-A062-518E800D2817}')!
JNIEnv comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Wrapper for JNI''s JNIEnv type.

Note: in JNI proper, there is one of these (at least) for each ''attatched'' client thread.  Since Dolphin is single-threaded (in the sense that it''s ''Processes'' do not correspond to Windows threads), Dolphin looks to the JVM like just one thread, hence we use just one JNIEnv per JavaVM (unless we are using
callbacks, in which case we need to maintain a stack of the things).

Also note that since these can never exist without a corresponding JavaVM, we add an instvar to record what that is.

-------------
	self instanceClass compileDefinition.
	self instanceClass generateVirtualMethods.
	self instanceClass generateCheckedMethods.
-------------
'!
!JNIEnv categoriesForClass!Unclassified! !
!JNIEnv methodsFor!

AllocObject_class: argument1
	"mechanically generated method to go via our vtable to invoke #AllocObject:class:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: JNIObject 28 JNIClass>
	^ self invalidCall.
!

AllocObject_class: argument1 onException: a1Block
	"mechanically generated method to invoke #AllocObject:class: and then check for exceptions"

	| answer |

	answer := self AllocObject_class: argument1.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

CallBooleanMethodA_obj: argument1 methodID: argument2 args: argument3
	"mechanically generated method to go via our vtable to invoke #CallBooleanMethodA:obj:methodID:args:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: byte 40 JNIObject JNIMethodID JNIValueArray*>
	^ self invalidCall.
!

CallBooleanMethodA_obj: argument1 methodID: argument2 args: argument3 onException: a1Block
	"mechanically generated method to invoke #CallBooleanMethodA:obj:methodID:args: and then check for exceptions"

	| answer |

	answer := self CallBooleanMethodA_obj: argument1 methodID: argument2 args: argument3.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

CallByteMethodA_obj: argument1 methodID: argument2 args: argument3
	"mechanically generated method to go via our vtable to invoke #CallByteMethodA:obj:methodID:args:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: sbyte 43 JNIObject JNIMethodID JNIValueArray*>
	^ self invalidCall.
!

CallByteMethodA_obj: argument1 methodID: argument2 args: argument3 onException: a1Block
	"mechanically generated method to invoke #CallByteMethodA:obj:methodID:args: and then check for exceptions"

	| answer |

	answer := self CallByteMethodA_obj: argument1 methodID: argument2 args: argument3.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

CallCharMethodA_obj: argument1 methodID: argument2 args: argument3
	"mechanically generated method to go via our vtable to invoke #CallCharMethodA:obj:methodID:args:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: word 46 JNIObject JNIMethodID JNIValueArray*>
	^ self invalidCall.
!

CallCharMethodA_obj: argument1 methodID: argument2 args: argument3 onException: a1Block
	"mechanically generated method to invoke #CallCharMethodA:obj:methodID:args: and then check for exceptions"

	| answer |

	answer := self CallCharMethodA_obj: argument1 methodID: argument2 args: argument3.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

CallDoubleMethodA_obj: argument1 methodID: argument2 args: argument3
	"mechanically generated method to go via our vtable to invoke #CallDoubleMethodA:obj:methodID:args:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: double 61 JNIObject JNIMethodID JNIValueArray*>
	^ self invalidCall.
!

CallDoubleMethodA_obj: argument1 methodID: argument2 args: argument3 onException: a1Block
	"mechanically generated method to invoke #CallDoubleMethodA:obj:methodID:args: and then check for exceptions"

	| answer |

	answer := self CallDoubleMethodA_obj: argument1 methodID: argument2 args: argument3.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

CallFloatMethodA_obj: argument1 methodID: argument2 args: argument3
	"mechanically generated method to go via our vtable to invoke #CallFloatMethodA:obj:methodID:args:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: float 58 JNIObject JNIMethodID JNIValueArray*>
	^ self invalidCall.
!

CallFloatMethodA_obj: argument1 methodID: argument2 args: argument3 onException: a1Block
	"mechanically generated method to invoke #CallFloatMethodA:obj:methodID:args: and then check for exceptions"

	| answer |

	answer := self CallFloatMethodA_obj: argument1 methodID: argument2 args: argument3.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

CallIntMethodA_obj: argument1 methodID: argument2 args: argument3
	"mechanically generated method to go via our vtable to invoke #CallIntMethodA:obj:methodID:args:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: sdword 52 JNIObject JNIMethodID JNIValueArray*>
	^ self invalidCall.
!

CallIntMethodA_obj: argument1 methodID: argument2 args: argument3 onException: a1Block
	"mechanically generated method to invoke #CallIntMethodA:obj:methodID:args: and then check for exceptions"

	| answer |

	answer := self CallIntMethodA_obj: argument1 methodID: argument2 args: argument3.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

CallLongMethodA_obj: argument1 methodID: argument2 args: argument3
	"mechanically generated method to go via our vtable to invoke #CallLongMethodA:obj:methodID:args:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: sqword 55 JNIObject JNIMethodID JNIValueArray*>
	^ self invalidCall.
!

CallLongMethodA_obj: argument1 methodID: argument2 args: argument3 onException: a1Block
	"mechanically generated method to invoke #CallLongMethodA:obj:methodID:args: and then check for exceptions"

	| answer |

	answer := self CallLongMethodA_obj: argument1 methodID: argument2 args: argument3.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

CallNonvirtualBooleanMethodA_obj: argument1 class: argument2 methodID: argument3 args: argument4
	"mechanically generated method to go via our vtable to invoke #CallNonvirtualBooleanMethodA:obj:class:methodID:args:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: byte 70 JNIObject JNIClass JNIMethodID JNIValueArray*>
	^ self invalidCall.
!

CallNonvirtualBooleanMethodA_obj: argument1 class: argument2 methodID: argument3 args: argument4 onException: a1Block
	"mechanically generated method to invoke #CallNonvirtualBooleanMethodA:obj:class:methodID:args: and then check for exceptions"

	| answer |

	answer := self CallNonvirtualBooleanMethodA_obj: argument1 class: argument2 methodID: argument3 args: argument4.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

CallNonvirtualByteMethodA_obj: argument1 class: argument2 methodID: argument3 args: argument4
	"mechanically generated method to go via our vtable to invoke #CallNonvirtualByteMethodA:obj:class:methodID:args:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: sbyte 73 JNIObject JNIClass JNIMethodID JNIValueArray*>
	^ self invalidCall.
!

CallNonvirtualByteMethodA_obj: argument1 class: argument2 methodID: argument3 args: argument4 onException: a1Block
	"mechanically generated method to invoke #CallNonvirtualByteMethodA:obj:class:methodID:args: and then check for exceptions"

	| answer |

	answer := self CallNonvirtualByteMethodA_obj: argument1 class: argument2 methodID: argument3 args: argument4.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

CallNonvirtualCharMethodA_obj: argument1 class: argument2 methodID: argument3 args: argument4
	"mechanically generated method to go via our vtable to invoke #CallNonvirtualCharMethodA:obj:class:methodID:args:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: word 76 JNIObject JNIClass JNIMethodID JNIValueArray*>
	^ self invalidCall.
!

CallNonvirtualCharMethodA_obj: argument1 class: argument2 methodID: argument3 args: argument4 onException: a1Block
	"mechanically generated method to invoke #CallNonvirtualCharMethodA:obj:class:methodID:args: and then check for exceptions"

	| answer |

	answer := self CallNonvirtualCharMethodA_obj: argument1 class: argument2 methodID: argument3 args: argument4.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

CallNonvirtualDoubleMethodA_obj: argument1 class: argument2 methodID: argument3 args: argument4
	"mechanically generated method to go via our vtable to invoke #CallNonvirtualDoubleMethodA:obj:class:methodID:args:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: double 91 JNIObject JNIClass JNIMethodID JNIValueArray*>
	^ self invalidCall.
!

CallNonvirtualDoubleMethodA_obj: argument1 class: argument2 methodID: argument3 args: argument4 onException: a1Block
	"mechanically generated method to invoke #CallNonvirtualDoubleMethodA:obj:class:methodID:args: and then check for exceptions"

	| answer |

	answer := self CallNonvirtualDoubleMethodA_obj: argument1 class: argument2 methodID: argument3 args: argument4.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

CallNonvirtualFloatMethodA_obj: argument1 class: argument2 methodID: argument3 args: argument4
	"mechanically generated method to go via our vtable to invoke #CallNonvirtualFloatMethodA:obj:class:methodID:args:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: float 88 JNIObject JNIClass JNIMethodID JNIValueArray*>
	^ self invalidCall.
!

CallNonvirtualFloatMethodA_obj: argument1 class: argument2 methodID: argument3 args: argument4 onException: a1Block
	"mechanically generated method to invoke #CallNonvirtualFloatMethodA:obj:class:methodID:args: and then check for exceptions"

	| answer |

	answer := self CallNonvirtualFloatMethodA_obj: argument1 class: argument2 methodID: argument3 args: argument4.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

CallNonvirtualIntMethodA_obj: argument1 class: argument2 methodID: argument3 args: argument4
	"mechanically generated method to go via our vtable to invoke #CallNonvirtualIntMethodA:obj:class:methodID:args:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: sdword 82 JNIObject JNIClass JNIMethodID JNIValueArray*>
	^ self invalidCall.
!

CallNonvirtualIntMethodA_obj: argument1 class: argument2 methodID: argument3 args: argument4 onException: a1Block
	"mechanically generated method to invoke #CallNonvirtualIntMethodA:obj:class:methodID:args: and then check for exceptions"

	| answer |

	answer := self CallNonvirtualIntMethodA_obj: argument1 class: argument2 methodID: argument3 args: argument4.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

CallNonvirtualLongMethodA_obj: argument1 class: argument2 methodID: argument3 args: argument4
	"mechanically generated method to go via our vtable to invoke #CallNonvirtualLongMethodA:obj:class:methodID:args:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: sqword 85 JNIObject JNIClass JNIMethodID JNIValueArray*>
	^ self invalidCall.
!

CallNonvirtualLongMethodA_obj: argument1 class: argument2 methodID: argument3 args: argument4 onException: a1Block
	"mechanically generated method to invoke #CallNonvirtualLongMethodA:obj:class:methodID:args: and then check for exceptions"

	| answer |

	answer := self CallNonvirtualLongMethodA_obj: argument1 class: argument2 methodID: argument3 args: argument4.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

CallNonvirtualObjectMethodA_obj: argument1 class: argument2 methodID: argument3 args: argument4
	"mechanically generated method to go via our vtable to invoke #CallNonvirtualObjectMethodA:obj:class:methodID:args:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: JNIObject 67 JNIObject JNIClass JNIMethodID JNIValueArray*>
	^ self invalidCall.
!

CallNonvirtualObjectMethodA_obj: argument1 class: argument2 methodID: argument3 args: argument4 onException: a1Block
	"mechanically generated method to invoke #CallNonvirtualObjectMethodA:obj:class:methodID:args: and then check for exceptions"

	| answer |

	answer := self CallNonvirtualObjectMethodA_obj: argument1 class: argument2 methodID: argument3 args: argument4.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

CallNonvirtualShortMethodA_obj: argument1 class: argument2 methodID: argument3 args: argument4
	"mechanically generated method to go via our vtable to invoke #CallNonvirtualShortMethodA:obj:class:methodID:args:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: sword 79 JNIObject JNIClass JNIMethodID JNIValueArray*>
	^ self invalidCall.
!

CallNonvirtualShortMethodA_obj: argument1 class: argument2 methodID: argument3 args: argument4 onException: a1Block
	"mechanically generated method to invoke #CallNonvirtualShortMethodA:obj:class:methodID:args: and then check for exceptions"

	| answer |

	answer := self CallNonvirtualShortMethodA_obj: argument1 class: argument2 methodID: argument3 args: argument4.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

CallNonvirtualVoidMethodA_obj: argument1 class: argument2 methodID: argument3 args: argument4
	"mechanically generated method to go via our vtable to invoke #CallNonvirtualVoidMethodA:obj:class:methodID:args:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: void 94 JNIObject JNIClass JNIMethodID JNIValueArray*>
	^ self invalidCall.
!

CallNonvirtualVoidMethodA_obj: argument1 class: argument2 methodID: argument3 args: argument4 onException: a1Block
	"mechanically generated method to invoke #CallNonvirtualVoidMethodA:obj:class:methodID:args: and then check for exceptions"

	| answer |

	answer := self CallNonvirtualVoidMethodA_obj: argument1 class: argument2 methodID: argument3 args: argument4.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

CallObjectMethodA_obj: argument1 methodID: argument2 args: argument3
	"mechanically generated method to go via our vtable to invoke #CallObjectMethodA:obj:methodID:args:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: JNIObject 37 JNIObject JNIMethodID JNIValueArray*>
	^ self invalidCall.
!

CallObjectMethodA_obj: argument1 methodID: argument2 args: argument3 onException: a1Block
	"mechanically generated method to invoke #CallObjectMethodA:obj:methodID:args: and then check for exceptions"

	| answer |

	answer := self CallObjectMethodA_obj: argument1 methodID: argument2 args: argument3.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

CallShortMethodA_obj: argument1 methodID: argument2 args: argument3
	"mechanically generated method to go via our vtable to invoke #CallShortMethodA:obj:methodID:args:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: sword 49 JNIObject JNIMethodID JNIValueArray*>
	^ self invalidCall.
!

CallShortMethodA_obj: argument1 methodID: argument2 args: argument3 onException: a1Block
	"mechanically generated method to invoke #CallShortMethodA:obj:methodID:args: and then check for exceptions"

	| answer |

	answer := self CallShortMethodA_obj: argument1 methodID: argument2 args: argument3.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

CallStaticBooleanMethodA_class: argument1 methodID: argument2 args: argument3
	"mechanically generated method to go via our vtable to invoke #CallStaticBooleanMethodA:class:methodID:args:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: byte 120 JNIClass JNIMethodID JNIValueArray*>
	^ self invalidCall.
!

CallStaticBooleanMethodA_class: argument1 methodID: argument2 args: argument3 onException: a1Block
	"mechanically generated method to invoke #CallStaticBooleanMethodA:class:methodID:args: and then check for exceptions"

	| answer |

	answer := self CallStaticBooleanMethodA_class: argument1 methodID: argument2 args: argument3.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

CallStaticByteMethodA_class: argument1 methodID: argument2 args: argument3
	"mechanically generated method to go via our vtable to invoke #CallStaticByteMethodA:class:methodID:args:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: sbyte 123 JNIClass JNIMethodID JNIValueArray*>
	^ self invalidCall.
!

CallStaticByteMethodA_class: argument1 methodID: argument2 args: argument3 onException: a1Block
	"mechanically generated method to invoke #CallStaticByteMethodA:class:methodID:args: and then check for exceptions"

	| answer |

	answer := self CallStaticByteMethodA_class: argument1 methodID: argument2 args: argument3.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

CallStaticCharMethodA_class: argument1 methodID: argument2 args: argument3
	"mechanically generated method to go via our vtable to invoke #CallStaticCharMethodA:class:methodID:args:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: word 126 JNIClass JNIMethodID JNIValueArray*>
	^ self invalidCall.
!

CallStaticCharMethodA_class: argument1 methodID: argument2 args: argument3 onException: a1Block
	"mechanically generated method to invoke #CallStaticCharMethodA:class:methodID:args: and then check for exceptions"

	| answer |

	answer := self CallStaticCharMethodA_class: argument1 methodID: argument2 args: argument3.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

CallStaticDoubleMethodA_class: argument1 methodID: argument2 args: argument3
	"mechanically generated method to go via our vtable to invoke #CallStaticDoubleMethodA:class:methodID:args:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: double 141 JNIClass JNIMethodID JNIValueArray*>
	^ self invalidCall.
!

CallStaticDoubleMethodA_class: argument1 methodID: argument2 args: argument3 onException: a1Block
	"mechanically generated method to invoke #CallStaticDoubleMethodA:class:methodID:args: and then check for exceptions"

	| answer |

	answer := self CallStaticDoubleMethodA_class: argument1 methodID: argument2 args: argument3.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

CallStaticFloatMethodA_class: argument1 methodID: argument2 args: argument3
	"mechanically generated method to go via our vtable to invoke #CallStaticFloatMethodA:class:methodID:args:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: float 138 JNIClass JNIMethodID JNIValueArray*>
	^ self invalidCall.
!

CallStaticFloatMethodA_class: argument1 methodID: argument2 args: argument3 onException: a1Block
	"mechanically generated method to invoke #CallStaticFloatMethodA:class:methodID:args: and then check for exceptions"

	| answer |

	answer := self CallStaticFloatMethodA_class: argument1 methodID: argument2 args: argument3.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

CallStaticIntMethodA_class: argument1 methodID: argument2 args: argument3
	"mechanically generated method to go via our vtable to invoke #CallStaticIntMethodA:class:methodID:args:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: sdword 132 JNIClass JNIMethodID JNIValueArray*>
	^ self invalidCall.
!

CallStaticIntMethodA_class: argument1 methodID: argument2 args: argument3 onException: a1Block
	"mechanically generated method to invoke #CallStaticIntMethodA:class:methodID:args: and then check for exceptions"

	| answer |

	answer := self CallStaticIntMethodA_class: argument1 methodID: argument2 args: argument3.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

CallStaticLongMethodA_class: argument1 methodID: argument2 args: argument3
	"mechanically generated method to go via our vtable to invoke #CallStaticLongMethodA:class:methodID:args:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: sqword 135 JNIClass JNIMethodID JNIValueArray*>
	^ self invalidCall.
!

CallStaticLongMethodA_class: argument1 methodID: argument2 args: argument3 onException: a1Block
	"mechanically generated method to invoke #CallStaticLongMethodA:class:methodID:args: and then check for exceptions"

	| answer |

	answer := self CallStaticLongMethodA_class: argument1 methodID: argument2 args: argument3.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

CallStaticObjectMethodA_class: argument1 methodID: argument2 args: argument3
	"mechanically generated method to go via our vtable to invoke #CallStaticObjectMethodA:class:methodID:args:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: JNIObject 117 JNIClass JNIMethodID JNIValueArray*>
	^ self invalidCall.
!

CallStaticObjectMethodA_class: argument1 methodID: argument2 args: argument3 onException: a1Block
	"mechanically generated method to invoke #CallStaticObjectMethodA:class:methodID:args: and then check for exceptions"

	| answer |

	answer := self CallStaticObjectMethodA_class: argument1 methodID: argument2 args: argument3.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

CallStaticShortMethodA_class: argument1 methodID: argument2 args: argument3
	"mechanically generated method to go via our vtable to invoke #CallStaticShortMethodA:class:methodID:args:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: sword 129 JNIClass JNIMethodID JNIValueArray*>
	^ self invalidCall.
!

CallStaticShortMethodA_class: argument1 methodID: argument2 args: argument3 onException: a1Block
	"mechanically generated method to invoke #CallStaticShortMethodA:class:methodID:args: and then check for exceptions"

	| answer |

	answer := self CallStaticShortMethodA_class: argument1 methodID: argument2 args: argument3.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

CallStaticVoidMethodA_class: argument1 methodID: argument2 args: argument3
	"mechanically generated method to go via our vtable to invoke #CallStaticVoidMethodA:class:methodID:args:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: void 144 JNIClass JNIMethodID JNIValueArray*>
	^ self invalidCall.
!

CallStaticVoidMethodA_class: argument1 methodID: argument2 args: argument3 onException: a1Block
	"mechanically generated method to invoke #CallStaticVoidMethodA:class:methodID:args: and then check for exceptions"

	| answer |

	answer := self CallStaticVoidMethodA_class: argument1 methodID: argument2 args: argument3.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

CallVoidMethodA_obj: argument1 methodID: argument2 args: argument3
	"mechanically generated method to go via our vtable to invoke #CallVoidMethodA:obj:methodID:args:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: void 64 JNIObject JNIMethodID JNIValueArray*>
	^ self invalidCall.
!

CallVoidMethodA_obj: argument1 methodID: argument2 args: argument3 onException: a1Block
	"mechanically generated method to invoke #CallVoidMethodA:obj:methodID:args: and then check for exceptions"

	| answer |

	answer := self CallVoidMethodA_obj: argument1 methodID: argument2 args: argument3.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

checkForException
	"answer whether there is a Java exception waiting to be handled"

	^ self ExceptionCheck == 1.!

DefineClass_name: argument1 loader: argument2 buf: argument3 len: argument4
	"mechanically generated method to go via our vtable to invoke #DefineClass:name:loader:buf:len:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: JNIClass 6 lpstr JNIObject lpvoid sdword>
	^ self invalidCall.
!

DefineClass_name: argument1 loader: argument2 buf: argument3 len: argument4 onException: a1Block
	"mechanically generated method to invoke #DefineClass:name:loader:buf:len: and then check for exceptions"

	| answer |

	answer := self DefineClass_name: argument1 loader: argument2 buf: argument3 len: argument4.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

DeleteGlobalRef_obj: argument1
	"mechanically generated method to go via our vtable to invoke #DeleteGlobalRef:obj:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: void 23 JNIObjectG>
	^ self invalidCall.
!

DeleteGlobalRef_obj: argument1 onException: a1Block
	"mechanically generated method to invoke #DeleteGlobalRef:obj: and then check for exceptions"

	| answer |

	answer := self DeleteGlobalRef_obj: argument1.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

DeleteLocalRef_obj: argument1
	"mechanically generated method to go via our vtable to invoke #DeleteLocalRef:obj:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: void 24 JNIObject>
	^ self invalidCall.
!

DeleteLocalRef_obj: argument1 onException: a1Block
	"mechanically generated method to invoke #DeleteLocalRef:obj: and then check for exceptions"

	| answer |

	answer := self DeleteLocalRef_obj: argument1.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

DeleteWeakGlobalRef_obj: argument1
	"mechanically generated method to go via our vtable to invoke #DeleteWeakGlobalRef:obj:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: void 228 JNIWeakGlobalRef>
	^ self invalidCall.
!

DeleteWeakGlobalRef_obj: argument1 onException: a1Block
	"mechanically generated method to invoke #DeleteWeakGlobalRef:obj: and then check for exceptions"

	| answer |

	answer := self DeleteWeakGlobalRef_obj: argument1.

	self ExceptionCheck == 1 ifTrue:
		[| jex | jex := self ExceptionOccurred. self ExceptionClear. ^ a1Block value: jex].

	^ answer.
!

EnsureLocalCapacity_capacity: argument1
	"mechanically generated method to go via our vtable to invoke #EnsureLocalCapacity:capacity:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: sdword 27 sdword>
	^ self invalidCall.
!

EnsureLocalCapacity_capacity: argument1 onException: a1Block
	"mechanically generated method to invoke #EnsureLocalCapacity:capacity: and then check for exceptions"

	| answer |

	answer := self EnsureLocalCapacity_capacity: argument1.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

ExceptionCheck
	"mechanically generated method to go via our vtable to invoke #ExceptionCheck:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: byte 229>
	^ self invalidCall.
!

ExceptionClear
	"mechanically generated method to go via our vtable to invoke #ExceptionClear:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: void 18>
	^ self invalidCall.
!

ExceptionDescribe
	"mechanically generated method to go via our vtable to invoke #ExceptionDescribe:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: void 17>
	^ self invalidCall.
!

exceptionDo: a1Block
	"on the assumption that there is a Java exception waiting to be handled, get the exception
	object, clear the exception state, and then answer the result of evaluating a1Block with
	the JNIThrowable as its parameter"

	| jex |

	jex := self ExceptionOccurred.
	self ExceptionClear.

	^ a1Block value: jex.!

ExceptionOccurred
	"mechanically generated method to go via our vtable to invoke #ExceptionOccurred:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: JNIThrowable 16>
	^ self invalidCall.
!

FatalError_msg: argument1
	"mechanically generated method to go via our vtable to invoke #FatalError:msg:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: void 19 lpstr>
	^ self invalidCall.
!

FatalError_msg: argument1 onException: a1Block
	"mechanically generated method to invoke #FatalError:msg: and then check for exceptions"

	| answer |

	answer := self FatalError_msg: argument1.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

FindClass_name: argument1
	"mechanically generated method to go via our vtable to invoke #FindClass:name:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: JNIClass 7 lpstr>
	^ self invalidCall.
!

FindClass_name: argument1 onException: a1Block
	"mechanically generated method to invoke #FindClass:name: and then check for exceptions"

	| answer |

	answer := self FindClass_name: argument1.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

FromReflectedField_field: argument1
	"mechanically generated method to go via our vtable to invoke #FromReflectedField:field:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: JNIFieldID 9 JNIObject>
	^ self invalidCall.
!

FromReflectedField_field: argument1 onException: a1Block
	"mechanically generated method to invoke #FromReflectedField:field: and then check for exceptions"

	| answer |

	answer := self FromReflectedField_field: argument1.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

FromReflectedMethod_method: argument1
	"mechanically generated method to go via our vtable to invoke #FromReflectedMethod:method:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: JNIMethodID 8 JNIObject>
	^ self invalidCall.
!

FromReflectedMethod_method: argument1 onException: a1Block
	"mechanically generated method to invoke #FromReflectedMethod:method: and then check for exceptions"

	| answer |

	answer := self FromReflectedMethod_method: argument1.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

GetArrayLength_array: argument1
	"mechanically generated method to go via our vtable to invoke #GetArrayLength:array:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: sdword 172 JNIArray>
	^ self invalidCall.
!

GetArrayLength_array: argument1 onException: a1Block
	"mechanically generated method to invoke #GetArrayLength:array: and then check for exceptions"

	| answer |

	answer := self GetArrayLength_array: argument1.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

GetBooleanArrayElements_array: argument1 isCopy: argument2
	"mechanically generated method to go via our vtable to invoke #GetBooleanArrayElements:array:isCopy:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: lpvoid 184 JNIBooleanArray lpvoid>
	^ self invalidCall.
!

GetBooleanArrayElements_array: argument1 isCopy: argument2 onException: a1Block
	"mechanically generated method to invoke #GetBooleanArrayElements:array:isCopy: and then check for exceptions"

	| answer |

	answer := self GetBooleanArrayElements_array: argument1 isCopy: argument2.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

GetBooleanArrayRegion_array: argument1 start: argument2 len: argument3 buf: argument4
	"mechanically generated method to go via our vtable to invoke #GetBooleanArrayRegion:array:start:len:buf:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: void 200 JNIBooleanArray sdword sdword lpvoid>
	^ self invalidCall.
!

GetBooleanArrayRegion_array: argument1 start: argument2 len: argument3 buf: argument4 onException: a1Block
	"mechanically generated method to invoke #GetBooleanArrayRegion:array:start:len:buf: and then check for exceptions"

	| answer |

	answer := self GetBooleanArrayRegion_array: argument1 start: argument2 len: argument3 buf: argument4.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

GetBooleanField_obj: argument1 fieldID: argument2
	"mechanically generated method to go via our vtable to invoke #GetBooleanField:obj:fieldID:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: byte 97 JNIObject JNIFieldID>
	^ self invalidCall.
!

GetBooleanField_obj: argument1 fieldID: argument2 onException: a1Block
	"mechanically generated method to invoke #GetBooleanField:obj:fieldID: and then check for exceptions"

	| answer |

	answer := self GetBooleanField_obj: argument1 fieldID: argument2.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

GetByteArrayElements_array: argument1 isCopy: argument2
	"mechanically generated method to go via our vtable to invoke #GetByteArrayElements:array:isCopy:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: lpvoid 185 JNIByteArray lpvoid>
	^ self invalidCall.
!

GetByteArrayElements_array: argument1 isCopy: argument2 onException: a1Block
	"mechanically generated method to invoke #GetByteArrayElements:array:isCopy: and then check for exceptions"

	| answer |

	answer := self GetByteArrayElements_array: argument1 isCopy: argument2.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

GetByteArrayRegion_array: argument1 start: argument2 len: argument3 buf: argument4
	"mechanically generated method to go via our vtable to invoke #GetByteArrayRegion:array:start:len:buf:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: void 201 JNIByteArray sdword sdword lpvoid>
	^ self invalidCall.
!

GetByteArrayRegion_array: argument1 start: argument2 len: argument3 buf: argument4 onException: a1Block
	"mechanically generated method to invoke #GetByteArrayRegion:array:start:len:buf: and then check for exceptions"

	| answer |

	answer := self GetByteArrayRegion_array: argument1 start: argument2 len: argument3 buf: argument4.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

GetByteField_obj: argument1 fieldID: argument2
	"mechanically generated method to go via our vtable to invoke #GetByteField:obj:fieldID:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: sbyte 98 JNIObject JNIFieldID>
	^ self invalidCall.
!

GetByteField_obj: argument1 fieldID: argument2 onException: a1Block
	"mechanically generated method to invoke #GetByteField:obj:fieldID: and then check for exceptions"

	| answer |

	answer := self GetByteField_obj: argument1 fieldID: argument2.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

GetCharArrayElements_array: argument1 isCopy: argument2
	"mechanically generated method to go via our vtable to invoke #GetCharArrayElements:array:isCopy:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: lpvoid 186 JNICharArray lpvoid>
	^ self invalidCall.
!

GetCharArrayElements_array: argument1 isCopy: argument2 onException: a1Block
	"mechanically generated method to invoke #GetCharArrayElements:array:isCopy: and then check for exceptions"

	| answer |

	answer := self GetCharArrayElements_array: argument1 isCopy: argument2.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

GetCharArrayRegion_array: argument1 start: argument2 len: argument3 buf: argument4
	"mechanically generated method to go via our vtable to invoke #GetCharArrayRegion:array:start:len:buf:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: void 202 JNICharArray sdword sdword WORD*>
	^ self invalidCall.
!

GetCharArrayRegion_array: argument1 start: argument2 len: argument3 buf: argument4 onException: a1Block
	"mechanically generated method to invoke #GetCharArrayRegion:array:start:len:buf: and then check for exceptions"

	| answer |

	answer := self GetCharArrayRegion_array: argument1 start: argument2 len: argument3 buf: argument4.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

GetCharField_obj: argument1 fieldID: argument2
	"mechanically generated method to go via our vtable to invoke #GetCharField:obj:fieldID:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: word 99 JNIObject JNIFieldID>
	^ self invalidCall.
!

GetCharField_obj: argument1 fieldID: argument2 onException: a1Block
	"mechanically generated method to invoke #GetCharField:obj:fieldID: and then check for exceptions"

	| answer |

	answer := self GetCharField_obj: argument1 fieldID: argument2.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

GetDirectBufferAddress_buf: argument1
	"mechanically generated method to go via our vtable to invoke #GetDirectBufferAddress:buf:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: lpvoid 231 JNIObject>
	^ self invalidCall.
!

GetDirectBufferAddress_buf: argument1 onException: a1Block
	"mechanically generated method to invoke #GetDirectBufferAddress:buf: and then check for exceptions"

	| answer |

	answer := self GetDirectBufferAddress_buf: argument1.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

GetDirectBufferCapacity_buf: argument1
	"mechanically generated method to go via our vtable to invoke #GetDirectBufferCapacity:buf:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: sqword 232 JNIObject>
	^ self invalidCall.
!

GetDirectBufferCapacity_buf: argument1 onException: a1Block
	"mechanically generated method to invoke #GetDirectBufferCapacity:buf: and then check for exceptions"

	| answer |

	answer := self GetDirectBufferCapacity_buf: argument1.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

GetDoubleArrayElements_array: argument1 isCopy: argument2
	"mechanically generated method to go via our vtable to invoke #GetDoubleArrayElements:array:isCopy:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: lpvoid 191 JNIDoubleArray lpvoid>
	^ self invalidCall.
!

GetDoubleArrayElements_array: argument1 isCopy: argument2 onException: a1Block
	"mechanically generated method to invoke #GetDoubleArrayElements:array:isCopy: and then check for exceptions"

	| answer |

	answer := self GetDoubleArrayElements_array: argument1 isCopy: argument2.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

GetDoubleArrayRegion_array: argument1 start: argument2 len: argument3 buf: argument4
	"mechanically generated method to go via our vtable to invoke #GetDoubleArrayRegion:array:start:len:buf:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: void 207 JNIDoubleArray sdword sdword DOUBLE*>
	^ self invalidCall.
!

GetDoubleArrayRegion_array: argument1 start: argument2 len: argument3 buf: argument4 onException: a1Block
	"mechanically generated method to invoke #GetDoubleArrayRegion:array:start:len:buf: and then check for exceptions"

	| answer |

	answer := self GetDoubleArrayRegion_array: argument1 start: argument2 len: argument3 buf: argument4.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

GetDoubleField_obj: argument1 fieldID: argument2
	"mechanically generated method to go via our vtable to invoke #GetDoubleField:obj:fieldID:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: double 104 JNIObject JNIFieldID>
	^ self invalidCall.
!

GetDoubleField_obj: argument1 fieldID: argument2 onException: a1Block
	"mechanically generated method to invoke #GetDoubleField:obj:fieldID: and then check for exceptions"

	| answer |

	answer := self GetDoubleField_obj: argument1 fieldID: argument2.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

GetFieldID_class: argument1 name: argument2 sig: argument3
	"mechanically generated method to go via our vtable to invoke #GetFieldID:class:name:sig:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: JNIFieldID 95 JNIClass lpstr lpstr>
	^ self invalidCall.
!

GetFieldID_class: argument1 name: argument2 sig: argument3 onException: a1Block
	"mechanically generated method to invoke #GetFieldID:class:name:sig: and then check for exceptions"

	| answer |

	answer := self GetFieldID_class: argument1 name: argument2 sig: argument3.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

GetFloatArrayElements_array: argument1 isCopy: argument2
	"mechanically generated method to go via our vtable to invoke #GetFloatArrayElements:array:isCopy:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: lpvoid 190 JNIFloatArray lpvoid>
	^ self invalidCall.
!

GetFloatArrayElements_array: argument1 isCopy: argument2 onException: a1Block
	"mechanically generated method to invoke #GetFloatArrayElements:array:isCopy: and then check for exceptions"

	| answer |

	answer := self GetFloatArrayElements_array: argument1 isCopy: argument2.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

GetFloatArrayRegion_array: argument1 start: argument2 len: argument3 buf: argument4
	"mechanically generated method to go via our vtable to invoke #GetFloatArrayRegion:array:start:len:buf:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: void 206 JNIFloatArray sdword sdword FLOAT*>
	^ self invalidCall.
!

GetFloatArrayRegion_array: argument1 start: argument2 len: argument3 buf: argument4 onException: a1Block
	"mechanically generated method to invoke #GetFloatArrayRegion:array:start:len:buf: and then check for exceptions"

	| answer |

	answer := self GetFloatArrayRegion_array: argument1 start: argument2 len: argument3 buf: argument4.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

GetFloatField_obj: argument1 fieldID: argument2
	"mechanically generated method to go via our vtable to invoke #GetFloatField:obj:fieldID:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: float 103 JNIObject JNIFieldID>
	^ self invalidCall.
!

GetFloatField_obj: argument1 fieldID: argument2 onException: a1Block
	"mechanically generated method to invoke #GetFloatField:obj:fieldID: and then check for exceptions"

	| answer |

	answer := self GetFloatField_obj: argument1 fieldID: argument2.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

GetIntArrayElements_array: argument1 isCopy: argument2
	"mechanically generated method to go via our vtable to invoke #GetIntArrayElements:array:isCopy:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: lpvoid 188 JNIIntArray lpvoid>
	^ self invalidCall.
!

GetIntArrayElements_array: argument1 isCopy: argument2 onException: a1Block
	"mechanically generated method to invoke #GetIntArrayElements:array:isCopy: and then check for exceptions"

	| answer |

	answer := self GetIntArrayElements_array: argument1 isCopy: argument2.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

GetIntArrayRegion_array: argument1 start: argument2 len: argument3 buf: argument4
	"mechanically generated method to go via our vtable to invoke #GetIntArrayRegion:array:start:len:buf:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: void 204 JNIIntArray sdword sdword SDWORD*>
	^ self invalidCall.
!

GetIntArrayRegion_array: argument1 start: argument2 len: argument3 buf: argument4 onException: a1Block
	"mechanically generated method to invoke #GetIntArrayRegion:array:start:len:buf: and then check for exceptions"

	| answer |

	answer := self GetIntArrayRegion_array: argument1 start: argument2 len: argument3 buf: argument4.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

GetIntField_obj: argument1 fieldID: argument2
	"mechanically generated method to go via our vtable to invoke #GetIntField:obj:fieldID:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: sdword 101 JNIObject JNIFieldID>
	^ self invalidCall.
!

GetIntField_obj: argument1 fieldID: argument2 onException: a1Block
	"mechanically generated method to invoke #GetIntField:obj:fieldID: and then check for exceptions"

	| answer |

	answer := self GetIntField_obj: argument1 fieldID: argument2.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

GetJavaVM_pvm: argument1
	"mechanically generated method to go via our vtable to invoke #GetJavaVM:pvm:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: sdword 220 lppvoid>
	^ self invalidCall.
!

GetJavaVM_pvm: argument1 onException: a1Block
	"mechanically generated method to invoke #GetJavaVM:pvm: and then check for exceptions"

	| answer |

	answer := self GetJavaVM_pvm: argument1.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

GetLongArrayElements_array: argument1 isCopy: argument2
	"mechanically generated method to go via our vtable to invoke #GetLongArrayElements:array:isCopy:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: lpvoid 189 JNILongArray lpvoid>
	^ self invalidCall.
!

GetLongArrayElements_array: argument1 isCopy: argument2 onException: a1Block
	"mechanically generated method to invoke #GetLongArrayElements:array:isCopy: and then check for exceptions"

	| answer |

	answer := self GetLongArrayElements_array: argument1 isCopy: argument2.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

GetLongArrayRegion_array: argument1 start: argument2 len: argument3 buf: argument4
	"mechanically generated method to go via our vtable to invoke #GetLongArrayRegion:array:start:len:buf:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: void 205 JNILongArray sdword sdword LARGE_INTEGER*>
	^ self invalidCall.
!

GetLongArrayRegion_array: argument1 start: argument2 len: argument3 buf: argument4 onException: a1Block
	"mechanically generated method to invoke #GetLongArrayRegion:array:start:len:buf: and then check for exceptions"

	| answer |

	answer := self GetLongArrayRegion_array: argument1 start: argument2 len: argument3 buf: argument4.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

GetLongField_obj: argument1 fieldID: argument2
	"mechanically generated method to go via our vtable to invoke #GetLongField:obj:fieldID:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: sqword 102 JNIObject JNIFieldID>
	^ self invalidCall.
!

GetLongField_obj: argument1 fieldID: argument2 onException: a1Block
	"mechanically generated method to invoke #GetLongField:obj:fieldID: and then check for exceptions"

	| answer |

	answer := self GetLongField_obj: argument1 fieldID: argument2.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

GetMethodID_class: argument1 name: argument2 sig: argument3
	"mechanically generated method to go via our vtable to invoke #GetMethodID:class:name:sig:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: JNIMethodID 34 JNIClass lpstr lpstr>
	^ self invalidCall.
!

GetMethodID_class: argument1 name: argument2 sig: argument3 onException: a1Block
	"mechanically generated method to invoke #GetMethodID:class:name:sig: and then check for exceptions"

	| answer |

	answer := self GetMethodID_class: argument1 name: argument2 sig: argument3.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

GetObjectArrayElement_array: argument1 index: argument2
	"mechanically generated method to go via our vtable to invoke #GetObjectArrayElement:array:index:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: JNIObject 174 JNIObjectArray sdword>
	^ self invalidCall.
!

GetObjectArrayElement_array: argument1 index: argument2 onException: a1Block
	"mechanically generated method to invoke #GetObjectArrayElement:array:index: and then check for exceptions"

	| answer |

	answer := self GetObjectArrayElement_array: argument1 index: argument2.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

GetObjectClass_obj: argument1
	"mechanically generated method to go via our vtable to invoke #GetObjectClass:obj:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: JNIClass 32 JNIObject>
	^ self invalidCall.
!

GetObjectClass_obj: argument1 onException: a1Block
	"mechanically generated method to invoke #GetObjectClass:obj: and then check for exceptions"

	| answer |

	answer := self GetObjectClass_obj: argument1.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

GetObjectField_obj: argument1 fieldID: argument2
	"mechanically generated method to go via our vtable to invoke #GetObjectField:obj:fieldID:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: JNIObject 96 JNIObject JNIFieldID>
	^ self invalidCall.
!

GetObjectField_obj: argument1 fieldID: argument2 onException: a1Block
	"mechanically generated method to invoke #GetObjectField:obj:fieldID: and then check for exceptions"

	| answer |

	answer := self GetObjectField_obj: argument1 fieldID: argument2.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

GetPrimitiveArrayCritical_array: argument1 isCopy: argument2
	"mechanically generated method to go via our vtable to invoke #GetPrimitiveArrayCritical:array:isCopy:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: lpvoid 223 JNIArray lpvoid>
	^ self invalidCall.
!

GetPrimitiveArrayCritical_array: argument1 isCopy: argument2 onException: a1Block
	"mechanically generated method to invoke #GetPrimitiveArrayCritical:array:isCopy: and then check for exceptions"

	| answer |

	answer := self GetPrimitiveArrayCritical_array: argument1 isCopy: argument2.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

GetShortArrayElements_array: argument1 isCopy: argument2
	"mechanically generated method to go via our vtable to invoke #GetShortArrayElements:array:isCopy:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: lpvoid 187 JNIShortArray lpvoid>
	^ self invalidCall.
!

GetShortArrayElements_array: argument1 isCopy: argument2 onException: a1Block
	"mechanically generated method to invoke #GetShortArrayElements:array:isCopy: and then check for exceptions"

	| answer |

	answer := self GetShortArrayElements_array: argument1 isCopy: argument2.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

GetShortArrayRegion_array: argument1 start: argument2 len: argument3 buf: argument4
	"mechanically generated method to go via our vtable to invoke #GetShortArrayRegion:array:start:len:buf:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: void 203 JNIShortArray sdword sdword SWORD*>
	^ self invalidCall.
!

GetShortArrayRegion_array: argument1 start: argument2 len: argument3 buf: argument4 onException: a1Block
	"mechanically generated method to invoke #GetShortArrayRegion:array:start:len:buf: and then check for exceptions"

	| answer |

	answer := self GetShortArrayRegion_array: argument1 start: argument2 len: argument3 buf: argument4.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

GetShortField_obj: argument1 fieldID: argument2
	"mechanically generated method to go via our vtable to invoke #GetShortField:obj:fieldID:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: sword 100 JNIObject JNIFieldID>
	^ self invalidCall.
!

GetShortField_obj: argument1 fieldID: argument2 onException: a1Block
	"mechanically generated method to invoke #GetShortField:obj:fieldID: and then check for exceptions"

	| answer |

	answer := self GetShortField_obj: argument1 fieldID: argument2.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

GetStaticBooleanField_class: argument1 fieldID: argument2
	"mechanically generated method to go via our vtable to invoke #GetStaticBooleanField:class:fieldID:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: byte 147 JNIClass JNIFieldID>
	^ self invalidCall.
!

GetStaticBooleanField_class: argument1 fieldID: argument2 onException: a1Block
	"mechanically generated method to invoke #GetStaticBooleanField:class:fieldID: and then check for exceptions"

	| answer |

	answer := self GetStaticBooleanField_class: argument1 fieldID: argument2.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

GetStaticByteField_class: argument1 fieldID: argument2
	"mechanically generated method to go via our vtable to invoke #GetStaticByteField:class:fieldID:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: sbyte 148 JNIClass JNIFieldID>
	^ self invalidCall.
!

GetStaticByteField_class: argument1 fieldID: argument2 onException: a1Block
	"mechanically generated method to invoke #GetStaticByteField:class:fieldID: and then check for exceptions"

	| answer |

	answer := self GetStaticByteField_class: argument1 fieldID: argument2.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

GetStaticCharField_class: argument1 fieldID: argument2
	"mechanically generated method to go via our vtable to invoke #GetStaticCharField:class:fieldID:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: word 149 JNIClass JNIFieldID>
	^ self invalidCall.
!

GetStaticCharField_class: argument1 fieldID: argument2 onException: a1Block
	"mechanically generated method to invoke #GetStaticCharField:class:fieldID: and then check for exceptions"

	| answer |

	answer := self GetStaticCharField_class: argument1 fieldID: argument2.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

GetStaticDoubleField_class: argument1 fieldID: argument2
	"mechanically generated method to go via our vtable to invoke #GetStaticDoubleField:class:fieldID:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: double 154 JNIClass JNIFieldID>
	^ self invalidCall.
!

GetStaticDoubleField_class: argument1 fieldID: argument2 onException: a1Block
	"mechanically generated method to invoke #GetStaticDoubleField:class:fieldID: and then check for exceptions"

	| answer |

	answer := self GetStaticDoubleField_class: argument1 fieldID: argument2.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

GetStaticFieldID_class: argument1 name: argument2 sig: argument3
	"mechanically generated method to go via our vtable to invoke #GetStaticFieldID:class:name:sig:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: JNIFieldID 145 JNIClass lpstr lpstr>
	^ self invalidCall.
!

GetStaticFieldID_class: argument1 name: argument2 sig: argument3 onException: a1Block
	"mechanically generated method to invoke #GetStaticFieldID:class:name:sig: and then check for exceptions"

	| answer |

	answer := self GetStaticFieldID_class: argument1 name: argument2 sig: argument3.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

GetStaticFloatField_class: argument1 fieldID: argument2
	"mechanically generated method to go via our vtable to invoke #GetStaticFloatField:class:fieldID:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: float 153 JNIClass JNIFieldID>
	^ self invalidCall.
!

GetStaticFloatField_class: argument1 fieldID: argument2 onException: a1Block
	"mechanically generated method to invoke #GetStaticFloatField:class:fieldID: and then check for exceptions"

	| answer |

	answer := self GetStaticFloatField_class: argument1 fieldID: argument2.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

GetStaticIntField_class: argument1 fieldID: argument2
	"mechanically generated method to go via our vtable to invoke #GetStaticIntField:class:fieldID:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: sdword 151 JNIClass JNIFieldID>
	^ self invalidCall.
!

GetStaticIntField_class: argument1 fieldID: argument2 onException: a1Block
	"mechanically generated method to invoke #GetStaticIntField:class:fieldID: and then check for exceptions"

	| answer |

	answer := self GetStaticIntField_class: argument1 fieldID: argument2.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

GetStaticLongField_class: argument1 fieldID: argument2
	"mechanically generated method to go via our vtable to invoke #GetStaticLongField:class:fieldID:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: sqword 152 JNIClass JNIFieldID>
	^ self invalidCall.
!

GetStaticLongField_class: argument1 fieldID: argument2 onException: a1Block
	"mechanically generated method to invoke #GetStaticLongField:class:fieldID: and then check for exceptions"

	| answer |

	answer := self GetStaticLongField_class: argument1 fieldID: argument2.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

GetStaticMethodID_class: argument1 name: argument2 sig: argument3
	"mechanically generated method to go via our vtable to invoke #GetStaticMethodID:class:name:sig:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: JNIMethodID 114 JNIClass lpstr lpstr>
	^ self invalidCall.
!

GetStaticMethodID_class: argument1 name: argument2 sig: argument3 onException: a1Block
	"mechanically generated method to invoke #GetStaticMethodID:class:name:sig: and then check for exceptions"

	| answer |

	answer := self GetStaticMethodID_class: argument1 name: argument2 sig: argument3.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

GetStaticObjectField_class: argument1 fieldID: argument2
	"mechanically generated method to go via our vtable to invoke #GetStaticObjectField:class:fieldID:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: JNIObject 146 JNIClass JNIFieldID>
	^ self invalidCall.
!

GetStaticObjectField_class: argument1 fieldID: argument2 onException: a1Block
	"mechanically generated method to invoke #GetStaticObjectField:class:fieldID: and then check for exceptions"

	| answer |

	answer := self GetStaticObjectField_class: argument1 fieldID: argument2.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

GetStaticShortField_class: argument1 fieldID: argument2
	"mechanically generated method to go via our vtable to invoke #GetStaticShortField:class:fieldID:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: sword 150 JNIClass JNIFieldID>
	^ self invalidCall.
!

GetStaticShortField_class: argument1 fieldID: argument2 onException: a1Block
	"mechanically generated method to invoke #GetStaticShortField:class:fieldID: and then check for exceptions"

	| answer |

	answer := self GetStaticShortField_class: argument1 fieldID: argument2.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

GetStringChars_str: argument1 isCopy: argument2
	"mechanically generated method to go via our vtable to invoke #GetStringChars:str:isCopy:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: lpvoid 166 JNIString lpvoid>
	^ self invalidCall.
!

GetStringChars_str: argument1 isCopy: argument2 onException: a1Block
	"mechanically generated method to invoke #GetStringChars:str:isCopy: and then check for exceptions"

	| answer |

	answer := self GetStringChars_str: argument1 isCopy: argument2.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

GetStringCritical_str: argument1 isCopy: argument2
	"mechanically generated method to go via our vtable to invoke #GetStringCritical:str:isCopy:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: lpvoid 225 JNIString lpvoid>
	^ self invalidCall.
!

GetStringCritical_str: argument1 isCopy: argument2 onException: a1Block
	"mechanically generated method to invoke #GetStringCritical:str:isCopy: and then check for exceptions"

	| answer |

	answer := self GetStringCritical_str: argument1 isCopy: argument2.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

GetStringLength_str: argument1
	"mechanically generated method to go via our vtable to invoke #GetStringLength:str:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: sdword 165 JNIString>
	^ self invalidCall.
!

GetStringLength_str: argument1 onException: a1Block
	"mechanically generated method to invoke #GetStringLength:str: and then check for exceptions"

	| answer |

	answer := self GetStringLength_str: argument1.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

GetStringRegion_str: argument1 start: argument2 len: argument3 buf: argument4
	"mechanically generated method to go via our vtable to invoke #GetStringRegion:str:start:len:buf:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: void 221 JNIString sdword sdword WORD*>
	^ self invalidCall.
!

GetStringRegion_str: argument1 start: argument2 len: argument3 buf: argument4 onException: a1Block
	"mechanically generated method to invoke #GetStringRegion:str:start:len:buf: and then check for exceptions"

	| answer |

	answer := self GetStringRegion_str: argument1 start: argument2 len: argument3 buf: argument4.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

GetStringUTFChars_str: argument1 isCopy: argument2
	"mechanically generated method to go via our vtable to invoke #GetStringUTFChars:str:isCopy:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: lpvoid 170 JNIString lpvoid>
	^ self invalidCall.
!

GetStringUTFChars_str: argument1 isCopy: argument2 onException: a1Block
	"mechanically generated method to invoke #GetStringUTFChars:str:isCopy: and then check for exceptions"

	| answer |

	answer := self GetStringUTFChars_str: argument1 isCopy: argument2.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

GetStringUTFLength_str: argument1
	"mechanically generated method to go via our vtable to invoke #GetStringUTFLength:str:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: sdword 169 JNIString>
	^ self invalidCall.
!

GetStringUTFLength_str: argument1 onException: a1Block
	"mechanically generated method to invoke #GetStringUTFLength:str: and then check for exceptions"

	| answer |

	answer := self GetStringUTFLength_str: argument1.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

GetStringUTFRegion_str: argument1 start: argument2 len: argument3 buf: argument4
	"mechanically generated method to go via our vtable to invoke #GetStringUTFRegion:str:start:len:buf:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: void 222 JNIString sdword sdword lpvoid>
	^ self invalidCall.
!

GetStringUTFRegion_str: argument1 start: argument2 len: argument3 buf: argument4 onException: a1Block
	"mechanically generated method to invoke #GetStringUTFRegion:str:start:len:buf: and then check for exceptions"

	| answer |

	answer := self GetStringUTFRegion_str: argument1 start: argument2 len: argument3 buf: argument4.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

GetSuperclass_sub: argument1
	"mechanically generated method to go via our vtable to invoke #GetSuperclass:sub:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: JNIClass 11 JNIClass>
	^ self invalidCall.
!

GetSuperclass_sub: argument1 onException: a1Block
	"mechanically generated method to invoke #GetSuperclass:sub: and then check for exceptions"

	| answer |

	answer := self GetSuperclass_sub: argument1.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

GetVersion
	"mechanically generated method to go via our vtable to invoke #GetVersion:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: sdword 5>
	^ self invalidCall.
!

ifExceptionDo: a1Block
	"if there is a Java exception waiting to be handled, get the exception object, clear the exception state,
	and then answer the result of evaluating a1Block with the JNIThrowable as its parameter.  Otherwise
	answer nil"

	"use the underling methods directly for speed -- this testing can be a hot spot"
	^ self ExceptionCheck == 1
		ifTrue:
			[| jex |
			jex := self ExceptionOccurred.
			self ExceptionClear.
			a1Block value: jex]
		ifFalse: [nil].!

IsAssignableFrom_sub: argument1 sup: argument2
	"mechanically generated method to go via our vtable to invoke #IsAssignableFrom:sub:sup:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: byte 12 JNIClass JNIClass>
	^ self invalidCall.
!

IsAssignableFrom_sub: argument1 sup: argument2 onException: a1Block
	"mechanically generated method to invoke #IsAssignableFrom:sub:sup: and then check for exceptions"

	| answer |

	answer := self IsAssignableFrom_sub: argument1 sup: argument2.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

isDead

	^ false.!

IsInstanceOf_obj: argument1 class: argument2
	"mechanically generated method to go via our vtable to invoke #IsInstanceOf:obj:class:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: byte 33 JNIObject JNIClass>
	^ self invalidCall.
!

IsInstanceOf_obj: argument1 class: argument2 onException: a1Block
	"mechanically generated method to invoke #IsInstanceOf:obj:class: and then check for exceptions"

	| answer |

	answer := self IsInstanceOf_obj: argument1 class: argument2.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

IsSameObject_obj: argument1 obj: argument2
	"mechanically generated method to go via our vtable to invoke #IsSameObject:obj:obj:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: byte 25 JNIObject JNIObject>
	^ self invalidCall.
!

IsSameObject_obj: argument1 obj: argument2 onException: a1Block
	"mechanically generated method to invoke #IsSameObject:obj:obj: and then check for exceptions"

	| answer |

	answer := self IsSameObject_obj: argument1 obj: argument2.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

javaVM
	"answer the receiver's owning JavaVM instance"

	^ javaVM.
!

javaVM: aJavaVM
	"private -- set the receiver's owning JavaVM instance"


	javaVM := aJavaVM.
!

MonitorEnter_obj: argument1
	"mechanically generated method to go via our vtable to invoke #MonitorEnter:obj:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: sdword 218 JNIObject>
	^ self invalidCall.
!

MonitorEnter_obj: argument1 onException: a1Block
	"mechanically generated method to invoke #MonitorEnter:obj: and then check for exceptions"

	| answer |

	answer := self MonitorEnter_obj: argument1.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

MonitorExit_obj: argument1
	"mechanically generated method to go via our vtable to invoke #MonitorExit:obj:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: sdword 219 JNIObject>
	^ self invalidCall.
!

MonitorExit_obj: argument1 onException: a1Block
	"mechanically generated method to invoke #MonitorExit:obj: and then check for exceptions"

	| answer |

	answer := self MonitorExit_obj: argument1.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

NewBooleanArray_len: argument1
	"mechanically generated method to go via our vtable to invoke #NewBooleanArray:len:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: JNIBooleanArray 176 sdword>
	^ self invalidCall.
!

NewBooleanArray_len: argument1 onException: a1Block
	"mechanically generated method to invoke #NewBooleanArray:len: and then check for exceptions"

	| answer |

	answer := self NewBooleanArray_len: argument1.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

NewByteArray_len: argument1
	"mechanically generated method to go via our vtable to invoke #NewByteArray:len:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: JNIByteArray 177 sdword>
	^ self invalidCall.
!

NewByteArray_len: argument1 onException: a1Block
	"mechanically generated method to invoke #NewByteArray:len: and then check for exceptions"

	| answer |

	answer := self NewByteArray_len: argument1.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

NewCharArray_len: argument1
	"mechanically generated method to go via our vtable to invoke #NewCharArray:len:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: JNICharArray 178 sdword>
	^ self invalidCall.
!

NewCharArray_len: argument1 onException: a1Block
	"mechanically generated method to invoke #NewCharArray:len: and then check for exceptions"

	| answer |

	answer := self NewCharArray_len: argument1.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

NewDirectByteBuffer_address: argument1 capacity: argument2
	"mechanically generated method to go via our vtable to invoke #NewDirectByteBuffer:address:capacity:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: JNIObject 230 lpvoid sqword>
	^ self invalidCall.
!

NewDirectByteBuffer_address: argument1 capacity: argument2 onException: a1Block
	"mechanically generated method to invoke #NewDirectByteBuffer:address:capacity: and then check for exceptions"

	| answer |

	answer := self NewDirectByteBuffer_address: argument1 capacity: argument2.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

NewDoubleArray_len: argument1
	"mechanically generated method to go via our vtable to invoke #NewDoubleArray:len:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: JNIDoubleArray 183 sdword>
	^ self invalidCall.
!

NewDoubleArray_len: argument1 onException: a1Block
	"mechanically generated method to invoke #NewDoubleArray:len: and then check for exceptions"

	| answer |

	answer := self NewDoubleArray_len: argument1.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

NewFloatArray_len: argument1
	"mechanically generated method to go via our vtable to invoke #NewFloatArray:len:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: JNIFloatArray 182 sdword>
	^ self invalidCall.
!

NewFloatArray_len: argument1 onException: a1Block
	"mechanically generated method to invoke #NewFloatArray:len: and then check for exceptions"

	| answer |

	answer := self NewFloatArray_len: argument1.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

NewGlobalRef_obj: argument1
	"mechanically generated method to go via our vtable to invoke #NewGlobalRef:obj:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: JNIObjectG 22 JNIObject>
	^ self invalidCall.
!

NewGlobalRef_obj: argument1 onException: a1Block
	"mechanically generated method to invoke #NewGlobalRef:obj: and then check for exceptions"

	| answer |

	answer := self NewGlobalRef_obj: argument1.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

NewIntArray_len: argument1
	"mechanically generated method to go via our vtable to invoke #NewIntArray:len:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: JNIIntArray 180 sdword>
	^ self invalidCall.
!

NewIntArray_len: argument1 onException: a1Block
	"mechanically generated method to invoke #NewIntArray:len: and then check for exceptions"

	| answer |

	answer := self NewIntArray_len: argument1.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

NewLocalRef_obj: argument1
	"mechanically generated method to go via our vtable to invoke #NewLocalRef:obj:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: JNIObject 26 JNIObjectG>
	^ self invalidCall.
!

NewLocalRef_obj: argument1 onException: a1Block
	"mechanically generated method to invoke #NewLocalRef:obj: and then check for exceptions"

	| answer |

	answer := self NewLocalRef_obj: argument1.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

NewLongArray_len: argument1
	"mechanically generated method to go via our vtable to invoke #NewLongArray:len:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: JNILongArray 181 sdword>
	^ self invalidCall.
!

NewLongArray_len: argument1 onException: a1Block
	"mechanically generated method to invoke #NewLongArray:len: and then check for exceptions"

	| answer |

	answer := self NewLongArray_len: argument1.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

NewObjectA_class: argument1 methodID: argument2 args: argument3
	"mechanically generated method to go via our vtable to invoke #NewObjectA:class:methodID:args:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: JNIObject 31 JNIClass JNIMethodID JNIValueArray*>
	^ self invalidCall.
!

NewObjectA_class: argument1 methodID: argument2 args: argument3 onException: a1Block
	"mechanically generated method to invoke #NewObjectA:class:methodID:args: and then check for exceptions"

	| answer |

	answer := self NewObjectA_class: argument1 methodID: argument2 args: argument3.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

NewObjectArray_len: argument1 class: argument2 init: argument3
	"mechanically generated method to go via our vtable to invoke #NewObjectArray:len:class:init:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: JNIObjectArray 173 sdword JNIClass JNIObject>
	^ self invalidCall.
!

NewObjectArray_len: argument1 class: argument2 init: argument3 onException: a1Block
	"mechanically generated method to invoke #NewObjectArray:len:class:init: and then check for exceptions"

	| answer |

	answer := self NewObjectArray_len: argument1 class: argument2 init: argument3.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

NewShortArray_len: argument1
	"mechanically generated method to go via our vtable to invoke #NewShortArray:len:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: JNIShortArray 179 sdword>
	^ self invalidCall.
!

NewShortArray_len: argument1 onException: a1Block
	"mechanically generated method to invoke #NewShortArray:len: and then check for exceptions"

	| answer |

	answer := self NewShortArray_len: argument1.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

NewString_unicode: argument1 len: argument2
	"mechanically generated method to go via our vtable to invoke #NewString:unicode:len:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: JNIString 164 WORD* sdword>
	^ self invalidCall.
!

NewString_unicode: argument1 len: argument2 onException: a1Block
	"mechanically generated method to invoke #NewString:unicode:len: and then check for exceptions"

	| answer |

	answer := self NewString_unicode: argument1 len: argument2.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

NewStringUTF_utf: argument1
	"mechanically generated method to go via our vtable to invoke #NewStringUTF:utf:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: JNIString 168 lpvoid>
	^ self invalidCall.
!

NewStringUTF_utf: argument1 onException: a1Block
	"mechanically generated method to invoke #NewStringUTF:utf: and then check for exceptions"

	| answer |

	answer := self NewStringUTF_utf: argument1.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

NewWeakGlobalRef_obj: argument1
	"mechanically generated method to go via our vtable to invoke #NewWeakGlobalRef:obj:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: JNIWeakGlobalRef 227 JNIObject>
	^ self invalidCall.
!

NewWeakGlobalRef_obj: argument1 onException: a1Block
	"mechanically generated method to invoke #NewWeakGlobalRef:obj: and then check for exceptions"

	| answer |

	answer := self NewWeakGlobalRef_obj: argument1.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

PopLocalFrame_result: argument1
	"mechanically generated method to go via our vtable to invoke #PopLocalFrame:result:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: JNIObject 21 JNIObject>
	^ self invalidCall.
!

PopLocalFrame_result: argument1 onException: a1Block
	"mechanically generated method to invoke #PopLocalFrame:result: and then check for exceptions"

	| answer |

	answer := self PopLocalFrame_result: argument1.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

PushLocalFrame_capacity: argument1
	"mechanically generated method to go via our vtable to invoke #PushLocalFrame:capacity:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: sdword 20 sdword>
	^ self invalidCall.
!

PushLocalFrame_capacity: argument1 onException: a1Block
	"mechanically generated method to invoke #PushLocalFrame:capacity: and then check for exceptions"

	| answer |

	answer := self PushLocalFrame_capacity: argument1.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

RegisterNatives_class: argument1 methods: argument2 nMethods: argument3
	"mechanically generated method to go via our vtable to invoke #RegisterNatives:class:methods:nMethods:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: sdword 216 JNIClass JNINativeMethod* sdword>
	^ self invalidCall.
!

RegisterNatives_class: argument1 methods: argument2 nMethods: argument3 onException: a1Block
	"mechanically generated method to invoke #RegisterNatives:class:methods:nMethods: and then check for exceptions"

	| answer |

	answer := self RegisterNatives_class: argument1 methods: argument2 nMethods: argument3.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

ReleaseBooleanArrayElements_array: argument1 elems: argument2 mode: argument3
	"mechanically generated method to go via our vtable to invoke #ReleaseBooleanArrayElements:array:elems:mode:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: void 192 JNIBooleanArray lpvoid sdword>
	^ self invalidCall.
!

ReleaseBooleanArrayElements_array: argument1 elems: argument2 mode: argument3 onException: a1Block
	"mechanically generated method to invoke #ReleaseBooleanArrayElements:array:elems:mode: and then check for exceptions"

	| answer |

	answer := self ReleaseBooleanArrayElements_array: argument1 elems: argument2 mode: argument3.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

ReleaseByteArrayElements_array: argument1 elems: argument2 mode: argument3
	"mechanically generated method to go via our vtable to invoke #ReleaseByteArrayElements:array:elems:mode:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: void 193 JNIByteArray lpvoid sdword>
	^ self invalidCall.
!

ReleaseByteArrayElements_array: argument1 elems: argument2 mode: argument3 onException: a1Block
	"mechanically generated method to invoke #ReleaseByteArrayElements:array:elems:mode: and then check for exceptions"

	| answer |

	answer := self ReleaseByteArrayElements_array: argument1 elems: argument2 mode: argument3.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

ReleaseCharArrayElements_array: argument1 elems: argument2 mode: argument3
	"mechanically generated method to go via our vtable to invoke #ReleaseCharArrayElements:array:elems:mode:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: void 194 JNICharArray lpvoid sdword>
	^ self invalidCall.
!

ReleaseCharArrayElements_array: argument1 elems: argument2 mode: argument3 onException: a1Block
	"mechanically generated method to invoke #ReleaseCharArrayElements:array:elems:mode: and then check for exceptions"

	| answer |

	answer := self ReleaseCharArrayElements_array: argument1 elems: argument2 mode: argument3.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

ReleaseDoubleArrayElements_array: argument1 elems: argument2 mode: argument3
	"mechanically generated method to go via our vtable to invoke #ReleaseDoubleArrayElements:array:elems:mode:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: void 199 JNIDoubleArray lpvoid sdword>
	^ self invalidCall.
!

ReleaseDoubleArrayElements_array: argument1 elems: argument2 mode: argument3 onException: a1Block
	"mechanically generated method to invoke #ReleaseDoubleArrayElements:array:elems:mode: and then check for exceptions"

	| answer |

	answer := self ReleaseDoubleArrayElements_array: argument1 elems: argument2 mode: argument3.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

ReleaseFloatArrayElements_array: argument1 elems: argument2 mode: argument3
	"mechanically generated method to go via our vtable to invoke #ReleaseFloatArrayElements:array:elems:mode:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: void 198 JNIFloatArray lpvoid sdword>
	^ self invalidCall.
!

ReleaseFloatArrayElements_array: argument1 elems: argument2 mode: argument3 onException: a1Block
	"mechanically generated method to invoke #ReleaseFloatArrayElements:array:elems:mode: and then check for exceptions"

	| answer |

	answer := self ReleaseFloatArrayElements_array: argument1 elems: argument2 mode: argument3.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

ReleaseIntArrayElements_array: argument1 elems: argument2 mode: argument3
	"mechanically generated method to go via our vtable to invoke #ReleaseIntArrayElements:array:elems:mode:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: void 196 JNIIntArray lpvoid sdword>
	^ self invalidCall.
!

ReleaseIntArrayElements_array: argument1 elems: argument2 mode: argument3 onException: a1Block
	"mechanically generated method to invoke #ReleaseIntArrayElements:array:elems:mode: and then check for exceptions"

	| answer |

	answer := self ReleaseIntArrayElements_array: argument1 elems: argument2 mode: argument3.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

ReleaseLongArrayElements_array: argument1 elems: argument2 mode: argument3
	"mechanically generated method to go via our vtable to invoke #ReleaseLongArrayElements:array:elems:mode:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: void 197 JNILongArray lpvoid sdword>
	^ self invalidCall.
!

ReleaseLongArrayElements_array: argument1 elems: argument2 mode: argument3 onException: a1Block
	"mechanically generated method to invoke #ReleaseLongArrayElements:array:elems:mode: and then check for exceptions"

	| answer |

	answer := self ReleaseLongArrayElements_array: argument1 elems: argument2 mode: argument3.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

ReleasePrimitiveArrayCritical_array: argument1 carray: argument2 mode: argument3
	"mechanically generated method to go via our vtable to invoke #ReleasePrimitiveArrayCritical:array:carray:mode:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: void 224 JNIArray lpvoid sdword>
	^ self invalidCall.
!

ReleasePrimitiveArrayCritical_array: argument1 carray: argument2 mode: argument3 onException: a1Block
	"mechanically generated method to invoke #ReleasePrimitiveArrayCritical:array:carray:mode: and then check for exceptions"

	| answer |

	answer := self ReleasePrimitiveArrayCritical_array: argument1 carray: argument2 mode: argument3.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

ReleaseShortArrayElements_array: argument1 elems: argument2 mode: argument3
	"mechanically generated method to go via our vtable to invoke #ReleaseShortArrayElements:array:elems:mode:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: void 195 JNIShortArray lpvoid sdword>
	^ self invalidCall.
!

ReleaseShortArrayElements_array: argument1 elems: argument2 mode: argument3 onException: a1Block
	"mechanically generated method to invoke #ReleaseShortArrayElements:array:elems:mode: and then check for exceptions"

	| answer |

	answer := self ReleaseShortArrayElements_array: argument1 elems: argument2 mode: argument3.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

ReleaseStringChars_str: argument1 chars: argument2
	"mechanically generated method to go via our vtable to invoke #ReleaseStringChars:str:chars:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: void 167 JNIString WORD*>
	^ self invalidCall.
!

ReleaseStringChars_str: argument1 chars: argument2 onException: a1Block
	"mechanically generated method to invoke #ReleaseStringChars:str:chars: and then check for exceptions"

	| answer |

	answer := self ReleaseStringChars_str: argument1 chars: argument2.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

ReleaseStringCritical_str: argument1 cstring: argument2
	"mechanically generated method to go via our vtable to invoke #ReleaseStringCritical:str:cstring:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: void 226 JNIString WORD*>
	^ self invalidCall.
!

ReleaseStringCritical_str: argument1 cstring: argument2 onException: a1Block
	"mechanically generated method to invoke #ReleaseStringCritical:str:cstring: and then check for exceptions"

	| answer |

	answer := self ReleaseStringCritical_str: argument1 cstring: argument2.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

ReleaseStringUTFChars_str: argument1 chars: argument2
	"mechanically generated method to go via our vtable to invoke #ReleaseStringUTFChars:str:chars:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: void 171 JNIString lpvoid>
	^ self invalidCall.
!

ReleaseStringUTFChars_str: argument1 chars: argument2 onException: a1Block
	"mechanically generated method to invoke #ReleaseStringUTFChars:str:chars: and then check for exceptions"

	| answer |

	answer := self ReleaseStringUTFChars_str: argument1 chars: argument2.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

SetBooleanArrayRegion_array: argument1 start: argument2 len: argument3 buf: argument4
	"mechanically generated method to go via our vtable to invoke #SetBooleanArrayRegion:array:start:len:buf:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: void 208 JNIBooleanArray sdword sdword lpvoid>
	^ self invalidCall.
!

SetBooleanArrayRegion_array: argument1 start: argument2 len: argument3 buf: argument4 onException: a1Block
	"mechanically generated method to invoke #SetBooleanArrayRegion:array:start:len:buf: and then check for exceptions"

	| answer |

	answer := self SetBooleanArrayRegion_array: argument1 start: argument2 len: argument3 buf: argument4.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

SetBooleanField_obj: argument1 fieldID: argument2 val: argument3
	"mechanically generated method to go via our vtable to invoke #SetBooleanField:obj:fieldID:val:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: void 106 JNIObject JNIFieldID byte>
	^ self invalidCall.
!

SetBooleanField_obj: argument1 fieldID: argument2 val: argument3 onException: a1Block
	"mechanically generated method to invoke #SetBooleanField:obj:fieldID:val: and then check for exceptions"

	| answer |

	answer := self SetBooleanField_obj: argument1 fieldID: argument2 val: argument3.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

SetByteArrayRegion_array: argument1 start: argument2 len: argument3 buf: argument4
	"mechanically generated method to go via our vtable to invoke #SetByteArrayRegion:array:start:len:buf:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: void 209 JNIByteArray sdword sdword lpvoid>
	^ self invalidCall.
!

SetByteArrayRegion_array: argument1 start: argument2 len: argument3 buf: argument4 onException: a1Block
	"mechanically generated method to invoke #SetByteArrayRegion:array:start:len:buf: and then check for exceptions"

	| answer |

	answer := self SetByteArrayRegion_array: argument1 start: argument2 len: argument3 buf: argument4.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

SetByteField_obj: argument1 fieldID: argument2 val: argument3
	"mechanically generated method to go via our vtable to invoke #SetByteField:obj:fieldID:val:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: void 107 JNIObject JNIFieldID sbyte>
	^ self invalidCall.
!

SetByteField_obj: argument1 fieldID: argument2 val: argument3 onException: a1Block
	"mechanically generated method to invoke #SetByteField:obj:fieldID:val: and then check for exceptions"

	| answer |

	answer := self SetByteField_obj: argument1 fieldID: argument2 val: argument3.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

SetCharArrayRegion_array: argument1 start: argument2 len: argument3 buf: argument4
	"mechanically generated method to go via our vtable to invoke #SetCharArrayRegion:array:start:len:buf:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: void 210 JNICharArray sdword sdword WORD*>
	^ self invalidCall.
!

SetCharArrayRegion_array: argument1 start: argument2 len: argument3 buf: argument4 onException: a1Block
	"mechanically generated method to invoke #SetCharArrayRegion:array:start:len:buf: and then check for exceptions"

	| answer |

	answer := self SetCharArrayRegion_array: argument1 start: argument2 len: argument3 buf: argument4.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

SetCharField_obj: argument1 fieldID: argument2 val: argument3
	"mechanically generated method to go via our vtable to invoke #SetCharField:obj:fieldID:val:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: void 108 JNIObject JNIFieldID word>
	^ self invalidCall.
!

SetCharField_obj: argument1 fieldID: argument2 val: argument3 onException: a1Block
	"mechanically generated method to invoke #SetCharField:obj:fieldID:val: and then check for exceptions"

	| answer |

	answer := self SetCharField_obj: argument1 fieldID: argument2 val: argument3.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

SetDoubleArrayRegion_array: argument1 start: argument2 len: argument3 buf: argument4
	"mechanically generated method to go via our vtable to invoke #SetDoubleArrayRegion:array:start:len:buf:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: void 215 JNIDoubleArray sdword sdword DOUBLE*>
	^ self invalidCall.
!

SetDoubleArrayRegion_array: argument1 start: argument2 len: argument3 buf: argument4 onException: a1Block
	"mechanically generated method to invoke #SetDoubleArrayRegion:array:start:len:buf: and then check for exceptions"

	| answer |

	answer := self SetDoubleArrayRegion_array: argument1 start: argument2 len: argument3 buf: argument4.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

SetDoubleField_obj: argument1 fieldID: argument2 val: argument3
	"mechanically generated method to go via our vtable to invoke #SetDoubleField:obj:fieldID:val:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: void 113 JNIObject JNIFieldID double>
	^ self invalidCall.
!

SetDoubleField_obj: argument1 fieldID: argument2 val: argument3 onException: a1Block
	"mechanically generated method to invoke #SetDoubleField:obj:fieldID:val: and then check for exceptions"

	| answer |

	answer := self SetDoubleField_obj: argument1 fieldID: argument2 val: argument3.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

SetFloatArrayRegion_array: argument1 start: argument2 len: argument3 buf: argument4
	"mechanically generated method to go via our vtable to invoke #SetFloatArrayRegion:array:start:len:buf:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: void 214 JNIFloatArray sdword sdword FLOAT*>
	^ self invalidCall.
!

SetFloatArrayRegion_array: argument1 start: argument2 len: argument3 buf: argument4 onException: a1Block
	"mechanically generated method to invoke #SetFloatArrayRegion:array:start:len:buf: and then check for exceptions"

	| answer |

	answer := self SetFloatArrayRegion_array: argument1 start: argument2 len: argument3 buf: argument4.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

SetFloatField_obj: argument1 fieldID: argument2 val: argument3
	"mechanically generated method to go via our vtable to invoke #SetFloatField:obj:fieldID:val:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: void 112 JNIObject JNIFieldID float>
	^ self invalidCall.
!

SetFloatField_obj: argument1 fieldID: argument2 val: argument3 onException: a1Block
	"mechanically generated method to invoke #SetFloatField:obj:fieldID:val: and then check for exceptions"

	| answer |

	answer := self SetFloatField_obj: argument1 fieldID: argument2 val: argument3.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

SetIntArrayRegion_array: argument1 start: argument2 len: argument3 buf: argument4
	"mechanically generated method to go via our vtable to invoke #SetIntArrayRegion:array:start:len:buf:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: void 212 JNIIntArray sdword sdword SDWORD*>
	^ self invalidCall.
!

SetIntArrayRegion_array: argument1 start: argument2 len: argument3 buf: argument4 onException: a1Block
	"mechanically generated method to invoke #SetIntArrayRegion:array:start:len:buf: and then check for exceptions"

	| answer |

	answer := self SetIntArrayRegion_array: argument1 start: argument2 len: argument3 buf: argument4.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

SetIntField_obj: argument1 fieldID: argument2 val: argument3
	"mechanically generated method to go via our vtable to invoke #SetIntField:obj:fieldID:val:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: void 110 JNIObject JNIFieldID sdword>
	^ self invalidCall.
!

SetIntField_obj: argument1 fieldID: argument2 val: argument3 onException: a1Block
	"mechanically generated method to invoke #SetIntField:obj:fieldID:val: and then check for exceptions"

	| answer |

	answer := self SetIntField_obj: argument1 fieldID: argument2 val: argument3.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

SetLongArrayRegion_array: argument1 start: argument2 len: argument3 buf: argument4
	"mechanically generated method to go via our vtable to invoke #SetLongArrayRegion:array:start:len:buf:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: void 213 JNILongArray sdword sdword LARGE_INTEGER*>
	^ self invalidCall.
!

SetLongArrayRegion_array: argument1 start: argument2 len: argument3 buf: argument4 onException: a1Block
	"mechanically generated method to invoke #SetLongArrayRegion:array:start:len:buf: and then check for exceptions"

	| answer |

	answer := self SetLongArrayRegion_array: argument1 start: argument2 len: argument3 buf: argument4.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

SetLongField_obj: argument1 fieldID: argument2 val: argument3
	"mechanically generated method to go via our vtable to invoke #SetLongField:obj:fieldID:val:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: void 111 JNIObject JNIFieldID sqword>
	^ self invalidCall.
!

SetLongField_obj: argument1 fieldID: argument2 val: argument3 onException: a1Block
	"mechanically generated method to invoke #SetLongField:obj:fieldID:val: and then check for exceptions"

	| answer |

	answer := self SetLongField_obj: argument1 fieldID: argument2 val: argument3.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

SetObjectArrayElement_array: argument1 index: argument2 val: argument3
	"mechanically generated method to go via our vtable to invoke #SetObjectArrayElement:array:index:val:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: void 175 JNIObjectArray sdword JNIObject>
	^ self invalidCall.
!

SetObjectArrayElement_array: argument1 index: argument2 val: argument3 onException: a1Block
	"mechanically generated method to invoke #SetObjectArrayElement:array:index:val: and then check for exceptions"

	| answer |

	answer := self SetObjectArrayElement_array: argument1 index: argument2 val: argument3.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

SetObjectField_obj: argument1 fieldID: argument2 val: argument3
	"mechanically generated method to go via our vtable to invoke #SetObjectField:obj:fieldID:val:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: void 105 JNIObject JNIFieldID JNIObject>
	^ self invalidCall.
!

SetObjectField_obj: argument1 fieldID: argument2 val: argument3 onException: a1Block
	"mechanically generated method to invoke #SetObjectField:obj:fieldID:val: and then check for exceptions"

	| answer |

	answer := self SetObjectField_obj: argument1 fieldID: argument2 val: argument3.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

SetShortArrayRegion_array: argument1 start: argument2 len: argument3 buf: argument4
	"mechanically generated method to go via our vtable to invoke #SetShortArrayRegion:array:start:len:buf:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: void 211 JNIShortArray sdword sdword SWORD*>
	^ self invalidCall.
!

SetShortArrayRegion_array: argument1 start: argument2 len: argument3 buf: argument4 onException: a1Block
	"mechanically generated method to invoke #SetShortArrayRegion:array:start:len:buf: and then check for exceptions"

	| answer |

	answer := self SetShortArrayRegion_array: argument1 start: argument2 len: argument3 buf: argument4.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

SetShortField_obj: argument1 fieldID: argument2 val: argument3
	"mechanically generated method to go via our vtable to invoke #SetShortField:obj:fieldID:val:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: void 109 JNIObject JNIFieldID sword>
	^ self invalidCall.
!

SetShortField_obj: argument1 fieldID: argument2 val: argument3 onException: a1Block
	"mechanically generated method to invoke #SetShortField:obj:fieldID:val: and then check for exceptions"

	| answer |

	answer := self SetShortField_obj: argument1 fieldID: argument2 val: argument3.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

SetStaticBooleanField_class: argument1 fieldID: argument2 val: argument3
	"mechanically generated method to go via our vtable to invoke #SetStaticBooleanField:class:fieldID:val:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: void 156 JNIClass JNIFieldID byte>
	^ self invalidCall.
!

SetStaticBooleanField_class: argument1 fieldID: argument2 val: argument3 onException: a1Block
	"mechanically generated method to invoke #SetStaticBooleanField:class:fieldID:val: and then check for exceptions"

	| answer |

	answer := self SetStaticBooleanField_class: argument1 fieldID: argument2 val: argument3.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

SetStaticByteField_class: argument1 fieldID: argument2 val: argument3
	"mechanically generated method to go via our vtable to invoke #SetStaticByteField:class:fieldID:val:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: void 157 JNIClass JNIFieldID sbyte>
	^ self invalidCall.
!

SetStaticByteField_class: argument1 fieldID: argument2 val: argument3 onException: a1Block
	"mechanically generated method to invoke #SetStaticByteField:class:fieldID:val: and then check for exceptions"

	| answer |

	answer := self SetStaticByteField_class: argument1 fieldID: argument2 val: argument3.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

SetStaticCharField_class: argument1 fieldID: argument2 val: argument3
	"mechanically generated method to go via our vtable to invoke #SetStaticCharField:class:fieldID:val:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: void 158 JNIClass JNIFieldID word>
	^ self invalidCall.
!

SetStaticCharField_class: argument1 fieldID: argument2 val: argument3 onException: a1Block
	"mechanically generated method to invoke #SetStaticCharField:class:fieldID:val: and then check for exceptions"

	| answer |

	answer := self SetStaticCharField_class: argument1 fieldID: argument2 val: argument3.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

SetStaticDoubleField_class: argument1 fieldID: argument2 val: argument3
	"mechanically generated method to go via our vtable to invoke #SetStaticDoubleField:class:fieldID:val:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: void 163 JNIClass JNIFieldID double>
	^ self invalidCall.
!

SetStaticDoubleField_class: argument1 fieldID: argument2 val: argument3 onException: a1Block
	"mechanically generated method to invoke #SetStaticDoubleField:class:fieldID:val: and then check for exceptions"

	| answer |

	answer := self SetStaticDoubleField_class: argument1 fieldID: argument2 val: argument3.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

SetStaticFloatField_class: argument1 fieldID: argument2 val: argument3
	"mechanically generated method to go via our vtable to invoke #SetStaticFloatField:class:fieldID:val:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: void 162 JNIClass JNIFieldID float>
	^ self invalidCall.
!

SetStaticFloatField_class: argument1 fieldID: argument2 val: argument3 onException: a1Block
	"mechanically generated method to invoke #SetStaticFloatField:class:fieldID:val: and then check for exceptions"

	| answer |

	answer := self SetStaticFloatField_class: argument1 fieldID: argument2 val: argument3.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

SetStaticIntField_class: argument1 fieldID: argument2 val: argument3
	"mechanically generated method to go via our vtable to invoke #SetStaticIntField:class:fieldID:val:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: void 160 JNIClass JNIFieldID sdword>
	^ self invalidCall.
!

SetStaticIntField_class: argument1 fieldID: argument2 val: argument3 onException: a1Block
	"mechanically generated method to invoke #SetStaticIntField:class:fieldID:val: and then check for exceptions"

	| answer |

	answer := self SetStaticIntField_class: argument1 fieldID: argument2 val: argument3.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

SetStaticLongField_class: argument1 fieldID: argument2 val: argument3
	"mechanically generated method to go via our vtable to invoke #SetStaticLongField:class:fieldID:val:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: void 161 JNIClass JNIFieldID sqword>
	^ self invalidCall.
!

SetStaticLongField_class: argument1 fieldID: argument2 val: argument3 onException: a1Block
	"mechanically generated method to invoke #SetStaticLongField:class:fieldID:val: and then check for exceptions"

	| answer |

	answer := self SetStaticLongField_class: argument1 fieldID: argument2 val: argument3.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

SetStaticObjectField_class: argument1 fieldID: argument2 val: argument3
	"mechanically generated method to go via our vtable to invoke #SetStaticObjectField:class:fieldID:val:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: void 155 JNIClass JNIFieldID JNIObject>
	^ self invalidCall.
!

SetStaticObjectField_class: argument1 fieldID: argument2 val: argument3 onException: a1Block
	"mechanically generated method to invoke #SetStaticObjectField:class:fieldID:val: and then check for exceptions"

	| answer |

	answer := self SetStaticObjectField_class: argument1 fieldID: argument2 val: argument3.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

SetStaticShortField_class: argument1 fieldID: argument2 val: argument3
	"mechanically generated method to go via our vtable to invoke #SetStaticShortField:class:fieldID:val:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: void 159 JNIClass JNIFieldID sword>
	^ self invalidCall.
!

SetStaticShortField_class: argument1 fieldID: argument2 val: argument3 onException: a1Block
	"mechanically generated method to invoke #SetStaticShortField:class:fieldID:val: and then check for exceptions"

	| answer |

	answer := self SetStaticShortField_class: argument1 fieldID: argument2 val: argument3.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

Throw_obj: argument1
	"mechanically generated method to go via our vtable to invoke #Throw:obj:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: sdword 14 JNIThrowable>
	^ self invalidCall.
!

Throw_obj: argument1 onException: a1Block
	"mechanically generated method to invoke #Throw:obj: and then check for exceptions"

	| answer |

	answer := self Throw_obj: argument1.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

ThrowNew_class: argument1 msg: argument2
	"mechanically generated method to go via our vtable to invoke #ThrowNew:class:msg:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: sdword 15 JNIClass lpstr>
	^ self invalidCall.
!

ThrowNew_class: argument1 msg: argument2 onException: a1Block
	"mechanically generated method to invoke #ThrowNew:class:msg: and then check for exceptions"

	| answer |

	answer := self ThrowNew_class: argument1 msg: argument2.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

ToReflectedField_class: argument1 fieldID: argument2 isStatic: argument3
	"mechanically generated method to go via our vtable to invoke #ToReflectedField:class:fieldID:isStatic:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: JNIObject 13 JNIClass JNIFieldID byte>
	^ self invalidCall.
!

ToReflectedField_class: argument1 fieldID: argument2 isStatic: argument3 onException: a1Block
	"mechanically generated method to invoke #ToReflectedField:class:fieldID:isStatic: and then check for exceptions"

	| answer |

	answer := self ToReflectedField_class: argument1 fieldID: argument2 isStatic: argument3.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

ToReflectedMethod_class: argument1 methodID: argument2 isStatic: argument3
	"mechanically generated method to go via our vtable to invoke #ToReflectedMethod:class:methodID:isStatic:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: JNIObject 10 JNIClass JNIMethodID byte>
	^ self invalidCall.
!

ToReflectedMethod_class: argument1 methodID: argument2 isStatic: argument3 onException: a1Block
	"mechanically generated method to invoke #ToReflectedMethod:class:methodID:isStatic: and then check for exceptions"

	| answer |

	answer := self ToReflectedMethod_class: argument1 methodID: argument2 isStatic: argument3.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

UnregisterNatives_class: argument1
	"mechanically generated method to go via our vtable to invoke #UnregisterNatives:class:.
	See: JNINativeInterface class>>functionPointerDefinitions"

	<virtual stdcall: sdword 217 JNIClass>
	^ self invalidCall.
!

UnregisterNatives_class: argument1 onException: a1Block
	"mechanically generated method to invoke #UnregisterNatives:class: and then check for exceptions"

	| answer |

	answer := self UnregisterNatives_class: argument1.

	^ self ExceptionCheck == 1
		ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]
		ifFalse: [answer].!

vtable
	"Answer the receiver's vtable field as a Smalltalk object."

	^JNINativeInterface fromAddress: (bytes sdwordAtOffset: 0).! !
!JNIEnv categoriesFor: #AllocObject_class:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #AllocObject_class:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #CallBooleanMethodA_obj:methodID:args:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #CallBooleanMethodA_obj:methodID:args:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #CallByteMethodA_obj:methodID:args:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #CallByteMethodA_obj:methodID:args:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #CallCharMethodA_obj:methodID:args:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #CallCharMethodA_obj:methodID:args:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #CallDoubleMethodA_obj:methodID:args:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #CallDoubleMethodA_obj:methodID:args:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #CallFloatMethodA_obj:methodID:args:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #CallFloatMethodA_obj:methodID:args:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #CallIntMethodA_obj:methodID:args:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #CallIntMethodA_obj:methodID:args:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #CallLongMethodA_obj:methodID:args:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #CallLongMethodA_obj:methodID:args:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #CallNonvirtualBooleanMethodA_obj:class:methodID:args:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #CallNonvirtualBooleanMethodA_obj:class:methodID:args:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #CallNonvirtualByteMethodA_obj:class:methodID:args:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #CallNonvirtualByteMethodA_obj:class:methodID:args:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #CallNonvirtualCharMethodA_obj:class:methodID:args:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #CallNonvirtualCharMethodA_obj:class:methodID:args:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #CallNonvirtualDoubleMethodA_obj:class:methodID:args:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #CallNonvirtualDoubleMethodA_obj:class:methodID:args:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #CallNonvirtualFloatMethodA_obj:class:methodID:args:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #CallNonvirtualFloatMethodA_obj:class:methodID:args:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #CallNonvirtualIntMethodA_obj:class:methodID:args:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #CallNonvirtualIntMethodA_obj:class:methodID:args:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #CallNonvirtualLongMethodA_obj:class:methodID:args:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #CallNonvirtualLongMethodA_obj:class:methodID:args:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #CallNonvirtualObjectMethodA_obj:class:methodID:args:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #CallNonvirtualObjectMethodA_obj:class:methodID:args:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #CallNonvirtualShortMethodA_obj:class:methodID:args:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #CallNonvirtualShortMethodA_obj:class:methodID:args:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #CallNonvirtualVoidMethodA_obj:class:methodID:args:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #CallNonvirtualVoidMethodA_obj:class:methodID:args:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #CallObjectMethodA_obj:methodID:args:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #CallObjectMethodA_obj:methodID:args:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #CallShortMethodA_obj:methodID:args:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #CallShortMethodA_obj:methodID:args:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #CallStaticBooleanMethodA_class:methodID:args:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #CallStaticBooleanMethodA_class:methodID:args:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #CallStaticByteMethodA_class:methodID:args:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #CallStaticByteMethodA_class:methodID:args:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #CallStaticCharMethodA_class:methodID:args:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #CallStaticCharMethodA_class:methodID:args:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #CallStaticDoubleMethodA_class:methodID:args:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #CallStaticDoubleMethodA_class:methodID:args:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #CallStaticFloatMethodA_class:methodID:args:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #CallStaticFloatMethodA_class:methodID:args:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #CallStaticIntMethodA_class:methodID:args:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #CallStaticIntMethodA_class:methodID:args:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #CallStaticLongMethodA_class:methodID:args:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #CallStaticLongMethodA_class:methodID:args:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #CallStaticObjectMethodA_class:methodID:args:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #CallStaticObjectMethodA_class:methodID:args:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #CallStaticShortMethodA_class:methodID:args:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #CallStaticShortMethodA_class:methodID:args:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #CallStaticVoidMethodA_class:methodID:args:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #CallStaticVoidMethodA_class:methodID:args:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #CallVoidMethodA_obj:methodID:args:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #CallVoidMethodA_obj:methodID:args:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #checkForException!exceptions!public! !
!JNIEnv categoriesFor: #DefineClass_name:loader:buf:len:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #DefineClass_name:loader:buf:len:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #DeleteGlobalRef_obj:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #DeleteGlobalRef_obj:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #DeleteLocalRef_obj:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #DeleteLocalRef_obj:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #DeleteWeakGlobalRef_obj:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #DeleteWeakGlobalRef_obj:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #EnsureLocalCapacity_capacity:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #EnsureLocalCapacity_capacity:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #ExceptionCheck!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #ExceptionClear!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #ExceptionDescribe!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #exceptionDo:!exceptions!public! !
!JNIEnv categoriesFor: #ExceptionOccurred!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #FatalError_msg:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #FatalError_msg:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #FindClass_name:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #FindClass_name:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #FromReflectedField_field:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #FromReflectedField_field:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #FromReflectedMethod_method:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #FromReflectedMethod_method:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #GetArrayLength_array:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #GetArrayLength_array:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #GetBooleanArrayElements_array:isCopy:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #GetBooleanArrayElements_array:isCopy:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #GetBooleanArrayRegion_array:start:len:buf:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #GetBooleanArrayRegion_array:start:len:buf:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #GetBooleanField_obj:fieldID:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #GetBooleanField_obj:fieldID:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #GetByteArrayElements_array:isCopy:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #GetByteArrayElements_array:isCopy:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #GetByteArrayRegion_array:start:len:buf:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #GetByteArrayRegion_array:start:len:buf:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #GetByteField_obj:fieldID:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #GetByteField_obj:fieldID:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #GetCharArrayElements_array:isCopy:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #GetCharArrayElements_array:isCopy:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #GetCharArrayRegion_array:start:len:buf:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #GetCharArrayRegion_array:start:len:buf:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #GetCharField_obj:fieldID:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #GetCharField_obj:fieldID:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #GetDirectBufferAddress_buf:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #GetDirectBufferAddress_buf:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #GetDirectBufferCapacity_buf:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #GetDirectBufferCapacity_buf:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #GetDoubleArrayElements_array:isCopy:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #GetDoubleArrayElements_array:isCopy:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #GetDoubleArrayRegion_array:start:len:buf:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #GetDoubleArrayRegion_array:start:len:buf:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #GetDoubleField_obj:fieldID:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #GetDoubleField_obj:fieldID:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #GetFieldID_class:name:sig:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #GetFieldID_class:name:sig:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #GetFloatArrayElements_array:isCopy:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #GetFloatArrayElements_array:isCopy:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #GetFloatArrayRegion_array:start:len:buf:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #GetFloatArrayRegion_array:start:len:buf:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #GetFloatField_obj:fieldID:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #GetFloatField_obj:fieldID:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #GetIntArrayElements_array:isCopy:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #GetIntArrayElements_array:isCopy:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #GetIntArrayRegion_array:start:len:buf:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #GetIntArrayRegion_array:start:len:buf:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #GetIntField_obj:fieldID:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #GetIntField_obj:fieldID:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #GetJavaVM_pvm:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #GetJavaVM_pvm:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #GetLongArrayElements_array:isCopy:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #GetLongArrayElements_array:isCopy:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #GetLongArrayRegion_array:start:len:buf:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #GetLongArrayRegion_array:start:len:buf:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #GetLongField_obj:fieldID:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #GetLongField_obj:fieldID:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #GetMethodID_class:name:sig:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #GetMethodID_class:name:sig:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #GetObjectArrayElement_array:index:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #GetObjectArrayElement_array:index:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #GetObjectClass_obj:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #GetObjectClass_obj:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #GetObjectField_obj:fieldID:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #GetObjectField_obj:fieldID:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #GetPrimitiveArrayCritical_array:isCopy:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #GetPrimitiveArrayCritical_array:isCopy:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #GetShortArrayElements_array:isCopy:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #GetShortArrayElements_array:isCopy:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #GetShortArrayRegion_array:start:len:buf:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #GetShortArrayRegion_array:start:len:buf:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #GetShortField_obj:fieldID:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #GetShortField_obj:fieldID:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #GetStaticBooleanField_class:fieldID:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #GetStaticBooleanField_class:fieldID:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #GetStaticByteField_class:fieldID:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #GetStaticByteField_class:fieldID:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #GetStaticCharField_class:fieldID:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #GetStaticCharField_class:fieldID:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #GetStaticDoubleField_class:fieldID:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #GetStaticDoubleField_class:fieldID:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #GetStaticFieldID_class:name:sig:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #GetStaticFieldID_class:name:sig:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #GetStaticFloatField_class:fieldID:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #GetStaticFloatField_class:fieldID:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #GetStaticIntField_class:fieldID:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #GetStaticIntField_class:fieldID:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #GetStaticLongField_class:fieldID:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #GetStaticLongField_class:fieldID:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #GetStaticMethodID_class:name:sig:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #GetStaticMethodID_class:name:sig:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #GetStaticObjectField_class:fieldID:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #GetStaticObjectField_class:fieldID:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #GetStaticShortField_class:fieldID:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #GetStaticShortField_class:fieldID:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #GetStringChars_str:isCopy:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #GetStringChars_str:isCopy:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #GetStringCritical_str:isCopy:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #GetStringCritical_str:isCopy:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #GetStringLength_str:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #GetStringLength_str:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #GetStringRegion_str:start:len:buf:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #GetStringRegion_str:start:len:buf:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #GetStringUTFChars_str:isCopy:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #GetStringUTFChars_str:isCopy:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #GetStringUTFLength_str:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #GetStringUTFLength_str:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #GetStringUTFRegion_str:start:len:buf:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #GetStringUTFRegion_str:start:len:buf:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #GetSuperclass_sub:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #GetSuperclass_sub:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #GetVersion!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #ifExceptionDo:!exceptions!public! !
!JNIEnv categoriesFor: #IsAssignableFrom_sub:sup:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #IsAssignableFrom_sub:sup:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #isDead!public!testing! !
!JNIEnv categoriesFor: #IsInstanceOf_obj:class:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #IsInstanceOf_obj:class:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #IsSameObject_obj:obj:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #IsSameObject_obj:obj:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #javaVM!accessing!public! !
!JNIEnv categoriesFor: #javaVM:!accessing!initializing!private! !
!JNIEnv categoriesFor: #MonitorEnter_obj:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #MonitorEnter_obj:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #MonitorExit_obj:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #MonitorExit_obj:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #NewBooleanArray_len:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #NewBooleanArray_len:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #NewByteArray_len:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #NewByteArray_len:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #NewCharArray_len:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #NewCharArray_len:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #NewDirectByteBuffer_address:capacity:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #NewDirectByteBuffer_address:capacity:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #NewDoubleArray_len:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #NewDoubleArray_len:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #NewFloatArray_len:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #NewFloatArray_len:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #NewGlobalRef_obj:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #NewGlobalRef_obj:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #NewIntArray_len:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #NewIntArray_len:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #NewLocalRef_obj:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #NewLocalRef_obj:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #NewLongArray_len:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #NewLongArray_len:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #NewObjectA_class:methodID:args:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #NewObjectA_class:methodID:args:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #NewObjectArray_len:class:init:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #NewObjectArray_len:class:init:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #NewShortArray_len:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #NewShortArray_len:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #NewString_unicode:len:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #NewString_unicode:len:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #NewStringUTF_utf:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #NewStringUTF_utf:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #NewWeakGlobalRef_obj:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #NewWeakGlobalRef_obj:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #PopLocalFrame_result:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #PopLocalFrame_result:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #PushLocalFrame_capacity:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #PushLocalFrame_capacity:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #RegisterNatives_class:methods:nMethods:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #RegisterNatives_class:methods:nMethods:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #ReleaseBooleanArrayElements_array:elems:mode:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #ReleaseBooleanArrayElements_array:elems:mode:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #ReleaseByteArrayElements_array:elems:mode:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #ReleaseByteArrayElements_array:elems:mode:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #ReleaseCharArrayElements_array:elems:mode:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #ReleaseCharArrayElements_array:elems:mode:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #ReleaseDoubleArrayElements_array:elems:mode:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #ReleaseDoubleArrayElements_array:elems:mode:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #ReleaseFloatArrayElements_array:elems:mode:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #ReleaseFloatArrayElements_array:elems:mode:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #ReleaseIntArrayElements_array:elems:mode:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #ReleaseIntArrayElements_array:elems:mode:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #ReleaseLongArrayElements_array:elems:mode:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #ReleaseLongArrayElements_array:elems:mode:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #ReleasePrimitiveArrayCritical_array:carray:mode:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #ReleasePrimitiveArrayCritical_array:carray:mode:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #ReleaseShortArrayElements_array:elems:mode:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #ReleaseShortArrayElements_array:elems:mode:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #ReleaseStringChars_str:chars:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #ReleaseStringChars_str:chars:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #ReleaseStringCritical_str:cstring:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #ReleaseStringCritical_str:cstring:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #ReleaseStringUTFChars_str:chars:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #ReleaseStringUTFChars_str:chars:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #SetBooleanArrayRegion_array:start:len:buf:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #SetBooleanArrayRegion_array:start:len:buf:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #SetBooleanField_obj:fieldID:val:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #SetBooleanField_obj:fieldID:val:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #SetByteArrayRegion_array:start:len:buf:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #SetByteArrayRegion_array:start:len:buf:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #SetByteField_obj:fieldID:val:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #SetByteField_obj:fieldID:val:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #SetCharArrayRegion_array:start:len:buf:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #SetCharArrayRegion_array:start:len:buf:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #SetCharField_obj:fieldID:val:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #SetCharField_obj:fieldID:val:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #SetDoubleArrayRegion_array:start:len:buf:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #SetDoubleArrayRegion_array:start:len:buf:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #SetDoubleField_obj:fieldID:val:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #SetDoubleField_obj:fieldID:val:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #SetFloatArrayRegion_array:start:len:buf:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #SetFloatArrayRegion_array:start:len:buf:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #SetFloatField_obj:fieldID:val:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #SetFloatField_obj:fieldID:val:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #SetIntArrayRegion_array:start:len:buf:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #SetIntArrayRegion_array:start:len:buf:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #SetIntField_obj:fieldID:val:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #SetIntField_obj:fieldID:val:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #SetLongArrayRegion_array:start:len:buf:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #SetLongArrayRegion_array:start:len:buf:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #SetLongField_obj:fieldID:val:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #SetLongField_obj:fieldID:val:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #SetObjectArrayElement_array:index:val:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #SetObjectArrayElement_array:index:val:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #SetObjectField_obj:fieldID:val:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #SetObjectField_obj:fieldID:val:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #SetShortArrayRegion_array:start:len:buf:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #SetShortArrayRegion_array:start:len:buf:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #SetShortField_obj:fieldID:val:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #SetShortField_obj:fieldID:val:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #SetStaticBooleanField_class:fieldID:val:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #SetStaticBooleanField_class:fieldID:val:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #SetStaticByteField_class:fieldID:val:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #SetStaticByteField_class:fieldID:val:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #SetStaticCharField_class:fieldID:val:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #SetStaticCharField_class:fieldID:val:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #SetStaticDoubleField_class:fieldID:val:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #SetStaticDoubleField_class:fieldID:val:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #SetStaticFloatField_class:fieldID:val:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #SetStaticFloatField_class:fieldID:val:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #SetStaticIntField_class:fieldID:val:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #SetStaticIntField_class:fieldID:val:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #SetStaticLongField_class:fieldID:val:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #SetStaticLongField_class:fieldID:val:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #SetStaticObjectField_class:fieldID:val:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #SetStaticObjectField_class:fieldID:val:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #SetStaticShortField_class:fieldID:val:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #SetStaticShortField_class:fieldID:val:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #Throw_obj:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #Throw_obj:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #ThrowNew_class:msg:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #ThrowNew_class:msg:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #ToReflectedField_class:fieldID:isStatic:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #ToReflectedField_class:fieldID:isStatic:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #ToReflectedMethod_class:methodID:isStatic:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #ToReflectedMethod_class:methodID:isStatic:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #UnregisterNatives_class:!**auto generated**!private!raw JNI functions! !
!JNIEnv categoriesFor: #UnregisterNatives_class:onException:!**auto generated**!checked JNI functions!public! !
!JNIEnv categoriesFor: #vtable!**compiled accessors**!public! !

!JNIEnv class methodsFor!

checkedMethodSourceFor: aSymbol
	"private -- answer the source for a method which self-sends aSymbol then checks for errors"

	^ (String writeStream: 400)
		display: (self virtualMethodPatternFor: aSymbol);
		display: ' onException: a1Block';
		crtab;
		display: '"mechanically generated method to invoke ';
		print: aSymbol;
		display: ' and then check for exceptions"';
		cr; crtab;
		display: '| answer |';
		cr; crtab;
		display: 'answer := self ';
		display: (self virtualMethodPatternFor: aSymbol);
		display: '.';
		cr; crtab;
		display: '^ self ExceptionCheck == 1';
		crtab; tab;
		display: 'ifTrue: [| jex | jex := self ExceptionOccurred. self ExceptionClear. a1Block value: jex]';
		crtab; tab;
		display: 'ifFalse: [answer].';
		contents.
!

defineCheckedMethodsInCategories: aCollectionOfCategories
	"private -- helps write the code"

	self vtableClass functionPointerDefinitions do:
		[:each || selector str |
		selector := each first.
		each second notNil ifTrue:
			[str := self checkedMethodSourceFor: selector.
			self compile: str categories: aCollectionOfCategories]].	
!

generateCheckedMethods
	"private -- helps write the code

		self generateCheckedMethods.
	"

	| categories |

	categories := Array
				with: (MethodCategory name: 'checked JNI functions')
				with: (MethodCategory name: '**auto generated**')
				with: MethodCategory public.

	self defineCheckedMethodsInCategories: categories.
!

new
	"private -- use #newWithJavaVM: aJavaVM"

	^ (super new).!

newWithJavaVM: aJavaVM
	"answer a new instance owned by the given JavaVM"

	^ (self new)
		javaVM: aJavaVM;
		yourself.!

vtableClass
	"private -- answer the class which defines the layout of our vtable"

	^ JNINativeInterface.! !
!JNIEnv class categoriesFor: #checkedMethodSourceFor:!generating methods!private! !
!JNIEnv class categoriesFor: #defineCheckedMethodsInCategories:!generating methods!private! !
!JNIEnv class categoriesFor: #generateCheckedMethods!generating methods!private! !
!JNIEnv class categoriesFor: #new!instance creation!private! !
!JNIEnv class categoriesFor: #newWithJavaVM:!instance creation!public! !
!JNIEnv class categoriesFor: #vtableClass!constants!private! !

"Binary Globals"!

"Resources"!

