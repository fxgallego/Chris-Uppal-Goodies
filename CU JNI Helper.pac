| package |
package := Package name: 'CU JNI Helper'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org

Wrapper for the DolphinJNIHelper DLL.  This provides a way of avoiding potential deadlocks when using the 3 hook functions provided by JNI for monitoring the state of a running JVM.

The DLL is the "Extras\DolphinJNIHelper.dll" file; source for it is in "Extras\DolphinJNIHelper.zip".  I advise rebuilding the DLL from source if you can, or at least checking the DLL with a good virus checker.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris'.

package basicPackageVersion: '1.00'.

package basicScriptAt: #postinstall put: '(Package manager packageNamed: ''CU JNI Helper'')
	propertyAt: #ExternalResourceFileNames
	put: #(
		''Extras\DolphinJNIHelper.zip''
		''Extras\DolphinJNIHelper.dll''
	).
!!'.

package classNames
	add: #DolphinJNIHelper;
	add: #DolphinJNIHelperLibrary;
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

ExternalLibrary subclass: #DolphinJNIHelperLibrary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalStructure subclass: #DolphinJNIHelper
	instanceVariableNames: 'library'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

DolphinJNIHelperLibrary guid: (GUID fromString: '{BD1EA9FB-0265-4884-A224-09CE4DF4F62D}')!
DolphinJNIHelperLibrary comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org
'!
!DolphinJNIHelperLibrary categoriesForClass!Unclassified! !
!DolphinJNIHelperLibrary methodsFor!

AvailableHelpers
	"private -- answer the number of helper structures that may still be created by GetJNIHelper.

		unsigned int __stdcall AvailableHelpers();
	"

	<stdcall: dword AvailableHelpers>
	self invalidCall.!

CallbackQueueLength: aDolphinJNIHelper
	"private -- call CallbackQueueLength() with aDolphinJNIHelper as parameter.

		unsigned __stdcall CallbackQueueLength(DolphinJNIHelper *);
	"

	<stdcall: dword CallbackQueueLength DolphinJNIHelper*>
	self invalidCall.
!

GetJNIHelper
	"private -- answer an external structure (which *must* be released with ReleaseJNIHandler(), that
	contains/accepts pointers to callback functions suitable for interfacing with JNI.
	
		DolphinJNIHelper *	__stdcall GetJNIHelper(void);
	"

	<stdcall: DolphinJNIHelper* GetJNIHelper>
	self invalidCall.!

getVFDBPrintf
	"private -- answer the address of the VFDBPrintf() function"

	^ self getProcAddress: 'VFDBPrintf'.!

makeJNIHelper
	"answer a new DolphinJNIHelper that is 'owned' by this library"

	| new |

	new := self GetJNIHelper.

	new isNil ifFalse: [new library: self].

	^ new.!

MaxCallbackQueueLength: aDolphinJNIHelper
	"private -- call MaxCallbackQueueLength() with aDolphinJNIHelper as parameter.

		unsigned __stdcall MaxCallbackQueueLength(DolphinJNIHelper *);
	"

	<stdcall: dword MaxCallbackQueueLength DolphinJNIHelper*>
	self invalidCall.
!

MaxHelpers
	"private -- answer the maximum number of helper structures that may be created by GetJNIHelper.

		unsigned int __stdcall MaxHelpers();
	"

	<stdcall: dword MaxHelpers>
	self invalidCall.!

ReleaseJNIHelper: aDolphinJNIHelper
	"private -- release a helper function created by GetJNIHelper.

		void __stdcall ReleaseJNIHelper(DolphinJNIHelper *);
	"

	<stdcall: void ReleaseJNIHelper DolphinJNIHelper *>
	self invalidCall.!

test
	"simple test that everthing's OK

		self default test.
	"

	| helper vfprintf exit abort |

	self assert: [self MaxHelpers > 0].
	self assert: [self AvailableHelpers > 0].
	self assert: [self AvailableHelpers <= self MaxHelpers].
	
	vfprintf := ExternalCallback
			block: [:fp :format :args | (format sprintfWithArguments: args) traceWith: 'vfprintf']
			descriptor: (ExternalDescriptor fromString: 'stdcall: sdword void* char* VaList*').
	exit := ExternalCallback
			block: [:code | code traceWith: 'exit']
			descriptor: (ExternalDescriptor fromString: 'stdcall: void sdword').
	abort := ExternalCallback
			block: [nil traceWith: 'abort']
			descriptor: (ExternalDescriptor fromString: 'stdcall: void').

	[helper := self GetJNIHelper.
	helper
		vfprintf_callback: vfprintf;
		exit_callback: exit;
		abort_callback: abort.
	self TestJNIHelperVFPrintf: helper.
	self TestJNIHelperExit: helper.
	self TestJNIHelperAbort: helper]
		ensure: [self ReleaseJNIHelper: helper].!

TestJNIHelperAbort: aDolphinJNIHelper
	"private -- exersize the Abort callback defined in aDolphinJNIHelper.

		void __stdcall TestJNIHelperAbort(DolphinJNIHelper *);
	"

	<stdcall: void TestJNIHelperAbort DolphinJNIHelper*>
	self invalidCall.
!

TestJNIHelperExit: aDolphinJNIHelper
	"private -- exersize the Exit callback defined in aDolphinJNIHelper.

		void __stdcall TestJNIHelperExit(DolphinJNIHelper *);
	"

	<stdcall: void TestJNIHelperExit DolphinJNIHelper*>
	self invalidCall.
!

TestJNIHelperVFPrintf: aDolphinJNIHelper
	"private -- exersize the VFPrintf callback defined in aDolphinJNIHelper.

		void __stdcall TestJNIHelperVFPrintf(DolphinJNIHelper *);
	"

	<stdcall: void TestJNIHelperVFPrintf DolphinJNIHelper*>
	self invalidCall.
! !
!DolphinJNIHelperLibrary categoriesFor: #AvailableHelpers!accessing!private! !
!DolphinJNIHelperLibrary categoriesFor: #CallbackQueueLength:!accessing!private! !
!DolphinJNIHelperLibrary categoriesFor: #GetJNIHelper!helpers!private! !
!DolphinJNIHelperLibrary categoriesFor: #getVFDBPrintf!accessing!private! !
!DolphinJNIHelperLibrary categoriesFor: #makeJNIHelper!helpers!public! !
!DolphinJNIHelperLibrary categoriesFor: #MaxCallbackQueueLength:!accessing!private! !
!DolphinJNIHelperLibrary categoriesFor: #MaxHelpers!accessing!private! !
!DolphinJNIHelperLibrary categoriesFor: #ReleaseJNIHelper:!helpers!private! !
!DolphinJNIHelperLibrary categoriesFor: #test!must strip!public!testing! !
!DolphinJNIHelperLibrary categoriesFor: #TestJNIHelperAbort:!helpers!private! !
!DolphinJNIHelperLibrary categoriesFor: #TestJNIHelperExit:!helpers!private! !
!DolphinJNIHelperLibrary categoriesFor: #TestJNIHelperVFPrintf:!helpers!private! !

!DolphinJNIHelperLibrary class methodsFor!

fileName
	"answer the file name of the external library which we represent"

	^ 'DolphinJNIHelper'
! !
!DolphinJNIHelperLibrary class categoriesFor: #fileName!constants!public! !

DolphinJNIHelper guid: (GUID fromString: '{455B6A14-F60C-4F4D-913E-C08767A040CC}')!
DolphinJNIHelper comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org

Simple wrapper for the "helper" structures used by the DolphinJNIHelper DLL.

Note that instances are finalisable and release the corresponding resources in the DLL as they die.'!
!DolphinJNIHelper categoriesForClass!Unclassified! !
!DolphinJNIHelper methodsFor!

abort_callback
	"Answer the receiver's abort_callback field as a Smalltalk object."

	^(bytes dwordAtOffset: 20) asExternalAddress!

abort_callback: anExternalCallback
	"set the receiver's abort_callback field to (the address of) anExternalCallback"

	"why do we need #asParameter *AND* #yourAddress here ???"
	bytes dwordAtOffset: 20 put: anExternalCallback asParameter yourAddress.!

abort_handler
	"Answer the receiver's abort_handler field as a Smalltalk object."

	^(bytes dwordAtOffset: 8) asExternalAddress!

basicFree
	"release the resources we represent"

	(library notNil and: [library isOpen]) ifTrue:
		[library ReleaseJNIHelper: self.
		library := nil].

	super basicFree.!

exit_callback
	"Answer the receiver's exit_callback field as a Smalltalk object."

	^(bytes dwordAtOffset: 16) asExternalAddress!

exit_callback: anExternalCallback
	"set the receiver's exit_callback field to (the address of) anExternalCallback"

	"why do we need #asParameter *AND* #yourAddress here ???"
	bytes dwordAtOffset: 16 put: anExternalCallback asParameter yourAddress.!

exit_handler
	"Answer the receiver's exit_handler field as a Smalltalk object."

	^(bytes dwordAtOffset: 4) asExternalAddress!

library: aDolphinJNIHelperLibrary
	"private -- set the owning library of this object"

	library := aDolphinJNIHelperLibrary.
	self beFinalizable.!

max_queue_length
	"Answer the receiver's max_queue_length field as a Smalltalk object."

	^(bytes dwordAtOffset: 32)!

maxQueueLength
	"answer the maximum length of the callback queue"

	"use the accessor function, rather than read the field directly, since the function is threadsafe"
	^ library isNil
		ifTrue: [0]
		ifFalse: [library MaxCallbackQueueLength: self].!

needsFree
	"answer (to the superclass's implementation of #free) whether it needs to call
	#basicFree"

	^ true.!

queue_length
	"Answer the receiver's queue_length field as a Smalltalk object."

	^(bytes dwordAtOffset: 28)!

queueLength
	"answer the current length of the queue"

	"use the accessor function, rather than read the field directly, since the function is threadsafe"
	^ library isNil
		ifTrue: [0]
		ifFalse: [library CallbackQueueLength: self].!

redirectedAbortCallback: aCallback
	"answer a new function pointer that can be passed to external libraries (specifically JNI)
	in place of aCallback, and arrange that when the library code uses the new one, from any
	thread, then aCallback will be invoked on the main Dolphin thread"

	self abort_callback: aCallback.
	^ self abort_handler.
!

redirectedExitCallback: aCallback
	"answer a new function pointer that can be passed to external libraries (specifically JNI)
	in place of aCallback, and arrange that when the library code uses the new one, from any
	thread, then aCallback will be invoked on the main Dolphin thread"

	self exit_callback: aCallback.
	^ self exit_handler.
!

redirectedVFPrintfCallback: aCallback
	"answer a new function pointer that can be passed to external libraries (specifically JNI)
	in place of aCallback, and arrange that when the library code uses the new one, from any
	thread, then aCallback will be invoked on the main Dolphin thread"

	self vfprintf_callback: aCallback.
	^ self vfprintf_handler.
!

systemDebugVFPrintfCallback
	"answer a new function pointer that can be passed to external libraries (specifically JNI)
	that has the same signature as VFPrintf() and which sends the text to the Windows debug
	stream.  Note, this never calls back into Dolphin at all, and so can be used as a less fragile
	(but less usefull) destination for JNU output"

	^ library getVFDBPrintf.
!

testAbort
	"run the underlying test function"

	library isNil ifFalse: [library TestJNIHelperAbort: self].!

testAll
	"run the underlying test function"

	self
		testAbort;
		testExit;
		testVFPrintf.!

testExit
	"run the underlying test function"

	library isNil ifFalse: [library TestJNIHelperExit: self].!

testVFPrintf
	"run the underlying test function"

	library isNil ifFalse: [library TestJNIHelperVFPrintf: self].!

thread_id
	"Answer the receiver's thread_id field as a Smalltalk object."

	^(bytes dwordAtOffset: 24)!

vfprintf_callback
	"Answer the receiver's vfprintf_callback field as a Smalltalk object."

	^(bytes dwordAtOffset: 12) asExternalAddress!

vfprintf_callback: anExternalCallback
	"set the receiver's vfprintf_callback field to (the address of) anExternalCallback"

	"why do we need #asParameter *AND* #yourAddress here ???"
	bytes dwordAtOffset: 12 put: anExternalCallback asParameter yourAddress.!

vfprintf_handler
	"Answer the receiver's vfprintf_handler field as a Smalltalk object."

	^(bytes dwordAtOffset: 0) asExternalAddress! !
!DolphinJNIHelper categoriesFor: #abort_callback!**compiled accessors**!public! !
!DolphinJNIHelper categoriesFor: #abort_callback:!accessing!public! !
!DolphinJNIHelper categoriesFor: #abort_handler!**compiled accessors**!public! !
!DolphinJNIHelper categoriesFor: #basicFree!operations!public!realizing/unrealizing! !
!DolphinJNIHelper categoriesFor: #exit_callback!**compiled accessors**!public! !
!DolphinJNIHelper categoriesFor: #exit_callback:!accessing!public! !
!DolphinJNIHelper categoriesFor: #exit_handler!**compiled accessors**!public! !
!DolphinJNIHelper categoriesFor: #library:!initialization!private! !
!DolphinJNIHelper categoriesFor: #max_queue_length!**compiled accessors**!public! !
!DolphinJNIHelper categoriesFor: #maxQueueLength!accessing!public! !
!DolphinJNIHelper categoriesFor: #needsFree!public!realizing/unrealizing! !
!DolphinJNIHelper categoriesFor: #queue_length!**compiled accessors**!public! !
!DolphinJNIHelper categoriesFor: #queueLength!accessing!public! !
!DolphinJNIHelper categoriesFor: #redirectedAbortCallback:!accessing!public! !
!DolphinJNIHelper categoriesFor: #redirectedExitCallback:!accessing!public! !
!DolphinJNIHelper categoriesFor: #redirectedVFPrintfCallback:!accessing!public! !
!DolphinJNIHelper categoriesFor: #systemDebugVFPrintfCallback!accessing!public! !
!DolphinJNIHelper categoriesFor: #testAbort!accessing!operations!public! !
!DolphinJNIHelper categoriesFor: #testAll!accessing!operations!public! !
!DolphinJNIHelper categoriesFor: #testExit!accessing!operations!public! !
!DolphinJNIHelper categoriesFor: #testVFPrintf!accessing!operations!public! !
!DolphinJNIHelper categoriesFor: #thread_id!**compiled accessors**!public! !
!DolphinJNIHelper categoriesFor: #vfprintf_callback!**compiled accessors**!public! !
!DolphinJNIHelper categoriesFor: #vfprintf_callback:!accessing!public! !
!DolphinJNIHelper categoriesFor: #vfprintf_handler!**compiled accessors**!public! !

!DolphinJNIHelper class methodsFor!

defineFields
	"
	self compileDefinition.

	typedef struct DolphinJNIHelper
	{
		long (__stdcall *vfprintf_handler)(FILE *fp, const char *format, va_list args);
		void (__stdcall *exit_handler)(long code);
		void (__stdcall *abort_handler)(void);

		long (__stdcall *vfprintf_callback)(FILE *fp, const char *format, va_list args);
		void (__stdcall *exit_callback)(long code);
		void (__stdcall *abort_callback)(void);

		unsigned long thread_id;
		unsigned int queue_length, max_queue_length;
	} DolphinJNIHelper;"

	self

		defineField: #vfprintf_handler type: LPVOIDField readOnly;
		defineField: #exit_handler type: LPVOIDField readOnly;
		defineField: #abort_handler type: LPVOIDField readOnly;

		defineField: #vfprintf_callback type: LPVOIDField new;
		defineField: #exit_callback type: LPVOIDField new;
		defineField: #abort_callback type: LPVOIDField new;

		defineField: #thread_id type: DWORDField readOnly;
		defineField: #queue_length type: DWORDField readOnly;
		defineField: #max_queue_length type: DWORDField readOnly;

		yourself.! !
!DolphinJNIHelper class categoriesFor: #defineFields!constants!public! !

"Binary Globals"!

"Resources"!

