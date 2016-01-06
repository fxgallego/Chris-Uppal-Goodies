| package |
package := Package name: 'CU Java Console Page'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2002 - 2005.
chris.uppal@metagnostic.org

Adds a console page to the JVM Status Monitor.  Requires callbacks to be installed and configured for it to do anything useful.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris'.

package basicPackageVersion: '1.02'.


package classNames
	add: #JVMConsolePage;
	add: #StaticOrgMetagnosticJniportJavaToDolphinOutputStream;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	add: #JVMConsolePage -> 'Default view';
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'CU Java Base';
	add: 'CU Java Callbacks';
	add: 'CU Java Status Monitor';
	add: 'CU PolyViewer';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Type Converters\Dolphin Type Converters';
	yourself).

package setManualPrerequisites: #(
	'CU Java Callbacks').

package!

"Class Definitions"!

StaticJavaLangObject subclass: #StaticOrgMetagnosticJniportJavaToDolphinOutputStream
	instanceVariableNames: 'callbacksAreRegistered'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JVMStatusPageAbstract subclass: #JVMConsolePage
	instanceVariableNames: 'stdout stderr queue mutex'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

StaticOrgMetagnosticJniportJavaToDolphinOutputStream guid: (GUID fromString: '{A8F751B9-3D9A-40E1-B482-7A98FAB19028}')!
StaticOrgMetagnosticJniportJavaToDolphinOutputStream comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org

Instances reify the Java class org.metagnostic.jniport.JavaToDolphinOutputStream and provide access to its class-side methods, which are most about replacing the Java console streams with instances of that class.  Thus allowing us to be passed any data that is written to java.lang.System.{out/err}.'!
!StaticOrgMetagnosticJniportJavaToDolphinOutputStream categoriesForClass!Unclassified! !
!StaticOrgMetagnosticJniportJavaToDolphinOutputStream methodsFor!

closeTag_null
	"answer the result of calling the receiver's public static closeTag() Java method"

	^ self callObjectMethod: 'closeTag' signature: '()Ljava/lang/Object;'.
!

ensureCallbacksAreRegistered
	"private -- try to ensure the registry knows about the callbacks we use.
	Answer whether we were successfull (may not be if callbacks aren't
	supported by this JVM)"

	| callbackRegistry |

	callbacksAreRegistered notNil ifTrue: [^ callbacksAreRegistered].

	callbackRegistry := self jvm callbackRegistry ifNil: [^ callbacksAreRegistered := false].

	callbackRegistry
		setCallback: (self writeTag_null) triggerWithOneArgument: #bytesWritten:;
		setCallback: (self closeTag_null) triggerWithNoArguments: #closed.

	^ callbacksAreRegistered := true.!

hasCanonicalInstancesByDefault
	"answer whether we should have canonical instances at startup.
	Overriden to force this"

	"we want instances to be canonical because events are triggered off
	them, and that does no good unless listeners are listening to the right
	object!!"
	^ true.!

hookStderr
	"answer an instance that replaces java.lang.System.err and off which
	#bytesWritten: and #closed will be triggered.
	If callbacks are not supported then answers nil"

	^ self ensureCallbacksAreRegistered
		ifTrue: [self installAsSystemErr_null]
		ifFalse: [nil].!

hookStdout
	"answer an instance that replaces java.lang.System.out and off which
	#bytesWritten: and #closed will be triggered.
	If callbacks are not supported then answers nil"

	^ self ensureCallbacksAreRegistered
		ifTrue: [self installAsSystemOut_null]
		ifFalse: [nil].!

installAsSystemErr_null
	"answer the result of calling the receiver's public static installAsSystemErr() Java method"

	^ self callObjectMethod: 'installAsSystemErr' signature: '()Lorg/metagnostic/jniport/JavaToDolphinOutputStream;'.
!

installAsSystemOut_null
	"answer the result of calling the receiver's public static synchronized installAsSystemOut() Java method"

	^ self callObjectMethod: 'installAsSystemOut' signature: '()Lorg/metagnostic/jniport/JavaToDolphinOutputStream;'.
!

isInstalledAsSystemErr_null
	"answer the result of calling the receiver's public static synchronized isInstalledAsSystemErr() Java method"

	^ self callBooleanMethod: 'isInstalledAsSystemErr'.
!

isInstalledAsSystemOut_null
	"answer the result of calling the receiver's public static synchronized isInstalledAsSystemOut() Java method"

	^ self callBooleanMethod: 'isInstalledAsSystemOut'.
!

new_null
	"answer the result of calling the receiver's public default Java constructor"

	^ self callConstructor.
!

notifyRegistered
	"this is called by the class registry once we have been fully initialised.
	Normally that means that the instance class and static class are both
	correct and stable (will not change unless you, the programmer, manually
	create and register a new wrapper class that is more appropriate for the
	Java class we represent.  If ghost classes are in use then we are fully
	populated with ghost methods by the time this is called too"

	super notifyRegistered.

	"this is just icing, discard any record of whether we've registered our callbacks"
	callbacksAreRegistered := nil.
!

removeAsSystemErr_null
	"invoke the receiver's public static synchronized removeAsSystemErr() Java method"

	self callVoidMethod: 'removeAsSystemErr'.
!

removeAsSystemOut_null
	"invoke the receiver's public static synchronized removeAsSystemOut() Java method"

	self callVoidMethod: 'removeAsSystemOut'.
!

stderrIsHooked
	"answer whether we currently have installed a replacement for the Java stderr stream"

	^ self isInstalledAsSystemErr_null.!

stdoutIsHooked
	"answer whether we currently have installed a replacement for the Java stdout stream"

	^ self isInstalledAsSystemOut_null.
!

unHookStderr
	"remove the hook for the Java stderr stream"

	self removeAsSystemErr_null.
!

unHookStdout
	"remove the hook for the Java stdout stream"

	self removeAsSystemOut_null.!

writeTag_null
	"answer the result of calling the receiver's public static writeTag() Java method"

	^ self callObjectMethod: 'writeTag' signature: '()Ljava/lang/Object;'.
! !
!StaticOrgMetagnosticJniportJavaToDolphinOutputStream categoriesFor: #closeTag_null!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticOrgMetagnosticJniportJavaToDolphinOutputStream categoriesFor: #ensureCallbacksAreRegistered!helpers!private! !
!StaticOrgMetagnosticJniportJavaToDolphinOutputStream categoriesFor: #hasCanonicalInstancesByDefault!canonical instances!constants!managed objects!public! !
!StaticOrgMetagnosticJniportJavaToDolphinOutputStream categoriesFor: #hookStderr!operations!public! !
!StaticOrgMetagnosticJniportJavaToDolphinOutputStream categoriesFor: #hookStdout!operations!public! !
!StaticOrgMetagnosticJniportJavaToDolphinOutputStream categoriesFor: #installAsSystemErr_null!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticOrgMetagnosticJniportJavaToDolphinOutputStream categoriesFor: #installAsSystemOut_null!**auto generated**!Java-methods!Java-public!Java-static!Java-synchronized!public! !
!StaticOrgMetagnosticJniportJavaToDolphinOutputStream categoriesFor: #isInstalledAsSystemErr_null!**auto generated**!Java-methods!Java-public!Java-static!Java-synchronized!public! !
!StaticOrgMetagnosticJniportJavaToDolphinOutputStream categoriesFor: #isInstalledAsSystemOut_null!**auto generated**!Java-methods!Java-public!Java-static!Java-synchronized!public! !
!StaticOrgMetagnosticJniportJavaToDolphinOutputStream categoriesFor: #new_null!**auto generated**!Java-constructors!Java-public!public! !
!StaticOrgMetagnosticJniportJavaToDolphinOutputStream categoriesFor: #notifyRegistered!canonical instances!initializing!public! !
!StaticOrgMetagnosticJniportJavaToDolphinOutputStream categoriesFor: #removeAsSystemErr_null!**auto generated**!Java-methods!Java-public!Java-static!Java-synchronized!public! !
!StaticOrgMetagnosticJniportJavaToDolphinOutputStream categoriesFor: #removeAsSystemOut_null!**auto generated**!Java-methods!Java-public!Java-static!Java-synchronized!public! !
!StaticOrgMetagnosticJniportJavaToDolphinOutputStream categoriesFor: #stderrIsHooked!public!testing! !
!StaticOrgMetagnosticJniportJavaToDolphinOutputStream categoriesFor: #stdoutIsHooked!public!testing! !
!StaticOrgMetagnosticJniportJavaToDolphinOutputStream categoriesFor: #unHookStderr!operations!public! !
!StaticOrgMetagnosticJniportJavaToDolphinOutputStream categoriesFor: #unHookStdout!operations!public! !
!StaticOrgMetagnosticJniportJavaToDolphinOutputStream categoriesFor: #writeTag_null!**auto generated**!Java-methods!Java-public!Java-static!public! !

!StaticOrgMetagnosticJniportJavaToDolphinOutputStream class methodsFor!

generatedConstructorSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
		#new_null
	).
!

generatedGetterSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
	).
!

generatedSetterSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
	).
!

generatedWrapperSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
		#closeTag_null
		#installAsSystemErr_null
		#installAsSystemOut_null
		#isInstalledAsSystemErr_null
		#isInstalledAsSystemOut_null
		#removeAsSystemErr_null
		#removeAsSystemOut_null
		#writeTag_null
	).
!

javaClassName
	"answer the Symbol name of the Java class we stand for"

	^ #'org.metagnostic.jniport.JavaToDolphinOutputStream'.
! !
!StaticOrgMetagnosticJniportJavaToDolphinOutputStream class categoriesFor: #generatedConstructorSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticOrgMetagnosticJniportJavaToDolphinOutputStream class categoriesFor: #generatedGetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticOrgMetagnosticJniportJavaToDolphinOutputStream class categoriesFor: #generatedSetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticOrgMetagnosticJniportJavaToDolphinOutputStream class categoriesFor: #generatedWrapperSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticOrgMetagnosticJniportJavaToDolphinOutputStream class categoriesFor: #javaClassName!**auto generated**!accessing!constants!public! !

JVMConsolePage guid: (GUID fromString: '{FE50DF11-83ED-44D6-99BD-D1B639440C9B}')!
JVMConsolePage comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org

A pluggin page for the JVM Status Monitor.

Uses the callback feature and a special subclass of java.io.OutputStream to replace the Java console streams (java.lang.System.{out/err} with ones which channel all the output into Dolphin.  It then picks up the output and displays it in the new "Console" page of the Status Monitor.

Installing this class will automatically add it to the Status Monitor.
'!
!JVMConsolePage categoriesForClass!Unclassified! !
!JVMConsolePage methodsFor!

addHooks
	"private -- set up the hooks that let us listen to the Java stdout and stderr"

	| hooker |

	self model jvmIsRunning ifFalse: [^ self].

	self model jvmSupportsCallbacks ifFalse: [^ self warnNoCallbacks].

	hooker := self findHookClass.

	(stdout := hooker hookStdout)
		when: #bytesWritten:
		send: #onBytesWrittenToStdout:
		to: self.
	(stderr := hooker hookStderr)
		when: #bytesWritten:
		send: #onBytesWrittenToStderr:
		to: self.!

addToTrace: aString color: aColor
	"private -- add aString to the end of our trace panel"

	(self tracePresenter view)
		caretPosition: 0;
		selectionColor: aColor;
		replaceSelection: aString.!

clearTrace
	"command -- clear any accumulated trace"

	self tracePresenter view clear.
!

createComponents
	"private - create presenters for the necessary components"

	super createComponents.

	self add: (RichTextPresenter on: nil) name: 'Trace'.

!

dequeue
	"private -- pull any queued notifications off the queue and paint them.  This is only called from
	the main thread"

	| next |

	[next := mutex critical: [queue removeFirstIfAbsent: [nil]].
	next isNil ifTrue: [^ self].
	self addToTrace: next first color: next second]
		repeat.!

enqueue: aString color: aColor
	"private -- add a notification to the queue"

	| entry kick |

	entry := Array with: aString with: aColor.

	mutex critical: [kick := queue isEmpty. queue addLast: entry].

	kick ifTrue: [SessionManager inputState queueDeferredAction: [self dequeue]].!

findHookClass
	"answer the Java static of the hook streams"

	^ self model jvm findJNIPortClass: #'org.metagnostic.jniport.JavaToDolphinOutputStream'.!

helpFileName
	"private -- answer the filename of our help text; this
	is relative to the documentation directory"

	^ 'status-monitor-console.html'.!

initialize
	"private -- establish a coherent initial state"

	queue := OrderedCollection new.
	mutex := Mutex new.

	super initialize.
!

model: aJVMStatusModel
	"private -- set the model for this Presenter"

#CUtodo.  "this is the point at which we should check whether the JVM supports callbacks"
#CUtodo.  "another bug -- we can get notified before wrapper classes, watchers, etc are fully enabled -- UGH!!!!"

	self removeHooks.
	super model: aJVMStatusModel.
	self clearTrace.
	self addHooks.
!

onBytesWrittenToStderr: aJavaByteArray
	"private -- someone has wrtten the given bytes to the Java stderr stream"

	self enqueue: aJavaByteArray asString color: self stderrColor.!

onBytesWrittenToStdout: aJavaByteArray
	"private -- someone has wrtten the given bytes to the Java stdout stream"

	self enqueue: aJavaByteArray asString color: self stdoutColor.!

onJVMIsLive
	"private -- the JVM has finished its intialisation sequence and is now fully live"

	super onJVMIsLive.

	self addHooks.!

onJVMReset
	"private -- the JVM has reset itself"

	"we need to re-establish contact with the hook class"
	self removeHooks; addHooks.!

onViewClosed
	"called by the system as our window is closed.
	Overridden to remove hooks"

	[self removeHooks]
		on: Exception
		do: [:err | ].

	super onViewClosed.
!

onViewOpened
	"called by the system as our window is opened"

	super onViewOpened.
	self model jvmSupportsCallbacks ifFalse: [self warnNoCallbacks].!

refresh
	"private -- refresh ourselves"

	"ignore it or else we'll clear our trace"!

removeHooks
	"private -- undo the effects of addHooks"

	| hooker |

	stdout isNil ifTrue: [^ self].
	self model jvmIsRunning ifFalse: [^ self].

	self findHookClass
		unHookStdout;
		unHookStderr.
	stdout ifNotNil: [:it | it removeEventsTriggeredFor: self].
	stderr ifNotNil: [:it | it removeEventsTriggeredFor: self].
	stdout := stderr := nil.
!

stderrColor
	"private -- answer the colour to use for mesages written to Java stderr"

	^ Color red.!

stdoutColor
	"private -- answer the colour to use for mesages written to Java stdout"

	^ Color black.!

tracePresenter
	"private -- answer the presenter named 'trace'"

	^ self presenterNamed: 'Trace'.
!

warnColor
	"private -- answer the colour to use for mesages written to Java stderr"

	^ Color yellow.!

warnNoCallbacks
	"private -- we have been started but the JVM doesn't support callbacks, and so
	cannot hook java.System.out and java.System.err"

	| msg |

	msg := 'This JVM instance does not support Java callbacks, and so cannot
divert the Java console streams.
'.

	self addToTrace: msg color: self warnColor.! !
!JVMConsolePage categoriesFor: #addHooks!helpers!private! !
!JVMConsolePage categoriesFor: #addToTrace:color:!helpers!private! !
!JVMConsolePage categoriesFor: #clearTrace!commands!public! !
!JVMConsolePage categoriesFor: #createComponents!initializing!private!subpresenters! !
!JVMConsolePage categoriesFor: #dequeue!helpers!private! !
!JVMConsolePage categoriesFor: #enqueue:color:!helpers!private! !
!JVMConsolePage categoriesFor: #findHookClass!helpers!private! !
!JVMConsolePage categoriesFor: #helpFileName!constants!private! !
!JVMConsolePage categoriesFor: #initialize!initializing!private! !
!JVMConsolePage categoriesFor: #model:!event handling!initializing!models!private! !
!JVMConsolePage categoriesFor: #onBytesWrittenToStderr:!event handling!private! !
!JVMConsolePage categoriesFor: #onBytesWrittenToStdout:!event handling!private! !
!JVMConsolePage categoriesFor: #onJVMIsLive!event handling!private! !
!JVMConsolePage categoriesFor: #onJVMReset!event handling!private! !
!JVMConsolePage categoriesFor: #onViewClosed!event handling!public! !
!JVMConsolePage categoriesFor: #onViewOpened!event handling!public! !
!JVMConsolePage categoriesFor: #refresh!commands!private! !
!JVMConsolePage categoriesFor: #removeHooks!helpers!private! !
!JVMConsolePage categoriesFor: #stderrColor!constants!private! !
!JVMConsolePage categoriesFor: #stdoutColor!constants!private! !
!JVMConsolePage categoriesFor: #tracePresenter!private!subpresenters! !
!JVMConsolePage categoriesFor: #warnColor!constants!private! !
!JVMConsolePage categoriesFor: #warnNoCallbacks!helpers!private! !

!JVMConsolePage class methodsFor!

initialize
	"private -- class initialization.

		self initialize.
	"

	| page |

	page := (PolyViewerPageDescription new)
			initiallyVisible: false;
			presenterClass: self;
			label: 'Console';
			yourself.

	JVMStatusShell addPolyViewerPage: page ownedBy: self.
!

uninitialize
	"private -- class tear-down.

		self uninitialize.
	"

	JVMStatusShell removePolyViewerPagesOwnedBy: self.
! !
!JVMConsolePage class categoriesFor: #initialize!initializing!private! !
!JVMConsolePage class categoriesFor: #uninitialize!initializing!private! !

"Binary Globals"!

"Resources"!

(ResourceIdentifier class: JVMConsolePage name: 'Default view') assign: (Object fromBinaryStoreBytes:
(ByteArray fromBase64String: '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
AJEBAACABQAAcgMAAAAAAAC6AAAAAAAAAFIAAAAMAAAAY29udGV4dE1lbnU6YgAAAAEAAAAQBgAA
gAUAAHIDAAAAAAAAQAQAAGIAAAABAAAABgEIAFJpY2hUZXh0AAAAAFIAAAB+AAAAe1xydGYxXGFu
c2lcYW5zaWNwZzEyNTJcZGVmZjBcZGVmbGFuZzIwNTd7XGZvbnR0Ymx7XGYwXGZyb21hblxmcHJx
MiBUaW1lcyBOZXcgUm9tYW47fX0NClx2aWV3a2luZDRcdWMxXHBhcmRcZjBcZnMyMiANClxwYXIg
fQ0KgAUAAHIDAAAAAAAAugAAAAAAAABSAAAADwAAAHNlbGVjdGlvblJhbmdlOmIAAAABAAAABgMI
AEludGVydmFsAAAAAAMAAAABAAAAAwAAAIAFAAByAwAAAAAAALoAAAAAAAAAUgAAAA8AAABpc1Rl
eHRNb2RpZmllZDpiAAAAAQAAACAAAACABQAAcgMAAAAAAAC6AAAAAAAAAFIAAAAPAAAAcmVzZXRD
aGFyRm9ybWF0YgAAAAAAAACABQAAggQAAAAAAAByAAAALAAAACwAAAAAAAAAAQAAAP//////////
//////////8FAAAABQAAAGMBAADNAAAAygAAAAAAAADQAAAAwAQAANAEAAAAAAAAEwAAAOIEAAAA
AAAAugAAAAAAAABSAAAADwAAAGZpeGVkUGFyZW50TGVmdAsAAAAgBQAA9////7oAAAAAAAAAUgAA
AA4AAABmaXhlZFBhcmVudFRvcAsAAABgBQAAu////+oAAAAAAAAAAAEAAGIAAAACAAAAgAUAAFIA
AAAFAAAAVHJhY2UAAAAAMgMAAAAAAADKAAAAAAAAANAAAABiAAAAAQAAAHIDAAAAAAAAkAMAAGIA
AAACAAAAwgMAAAAAAAALAAAACwAAAMIDAAAAAAAA0QIAAOEBAACgAQAAggQAAAAAAAByAAAALAAA
ACwAAAAAAAAAAAAAAP////////////////////8FAAAABQAAAG0BAAD1AAAAygAAAAAAAADQAAAA
YgAAAAIAAACABQAAcAIAANAEAAAAAAAAEwAAAEYFBAADAAAASWNvbgAAAAAAAAAAEAAAAA4CEQBT
VEJTaW5nbGV0b25Qcm94eQAAAACaAAAAAAAAAFIAAAAHAAAARG9scGhpblIAAAAYAAAASW1hZ2VS
ZWxhdGl2ZUZpbGVMb2NhdG9yugAAAAAAAABSAAAABwAAAGN1cnJlbnRSAAAAEQAAAENvbnRhaW5l
clZpZXcuaWNvDgIfAFNUQkV4dGVybmFsUmVzb3VyY2VMaWJyYXJ5UHJveHkAAAAAUgAAABAAAABk
b2xwaGluZHIwMDUuZGxsAAAAAA=='))!

