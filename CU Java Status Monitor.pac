| package |
package := Package name: 'CU Java Status Monitor'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2002 - 2005.
chris.uppal@metagnostic.org

The Status Monitor is a GUI tool for viewing the state of any JVMs that have been started by JNIPort.

It is actually a container object with pluggable "pages", each of which display some aspect of the status.  Some core pages are included in this package.  Others are in ''CU Java Console Page'' and ''CU Java History Page''.

Also has features for editing and saving the predefined JVMStatus settings.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris'.

package basicPackageVersion: '1.23'.


package classNames
	add: #FakeJavaStatic;
	add: #JavaClassHierarchyModel;
	add: #JVMClassesPage;
	add: #JVMClassloadersPage;
	add: #JVMDBPrintfPage;
	add: #JVMHistoryItem;
	add: #JVMStatusModel;
	add: #JVMStatusMonitorSettings;
	add: #JVMStatusPage;
	add: #JVMStatusPageAbstract;
	add: #JVMStatusShell;
	add: #SupplementaryClassloaderTreeModel;
	yourself.

package methodNames
	add: #JavaArrayClassStatic -> #browserString;
	add: #JavaArrayClassStatic -> #canBeBrowsed;
	add: #JavaArrayClassStatic -> #canBeWrapped;
	add: #JavaPrimitiveStatic -> #canBeBrowsed;
	add: #JavaPrimitiveStatic -> #canBeWrapped;
	add: #JavaStatic -> #browserString;
	add: #JavaStatic -> #canBeBrowsed;
	add: #JavaStatic -> #canBeWrapped;
	add: #JavaStatic -> #generatedInstanceSelectors;
	add: #JavaStatic -> #generatedSelectors;
	add: #JavaStatic -> #isFake;
	add: #JVMSettings -> #statusMonitorSettings;
	add: #SupplementaryClassloader -> #icon;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	add: #JVMClassesPage -> 'Default view';
	add: #JVMClassloadersPage -> 'Default view';
	add: #JVMDBPrintfPage -> 'Default view';
	add: #JVMStatusPage -> 'Default view';
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'CU Enhanced Scrolling Decorator';
	add: 'CU Form Layout';
	add: 'CU Java Base';
	add: 'CU JNI';
	add: 'CU Package-relative File Locator';
	add: 'CU PolyViewer';
	add: 'CU PolyViewer Tools Base';
	add: 'CU Rolling Accumulator';
	add: 'CU Sortblocks';
	add: 'CU Windows Shell Extensions';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Views\Common Controls\Dolphin Common Controls';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Models\Tree\Dolphin Tree Models';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Type Converters\Dolphin Type Converters';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Models\Value\Dolphin Value Models';
	yourself).

package setManualPrerequisites: #(
	'CU Windows Shell Extensions').

package!

"Class Definitions"!

Object subclass: #FakeJavaStatic
	instanceVariableNames: 'name description'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #JVMHistoryItem
	instanceVariableNames: 'localRefs globalRefs births deaths callbacks duration'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'noData'!
JVMSubSettings subclass: #JVMStatusMonitorSettings
	instanceVariableNames: 'javadocPath defaultUpdateInterval defaultSampleSize'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Model subclass: #JVMStatusModel
	instanceVariableNames: 'jvm statusList history lastObjectsCreated lastObjectsReleased lastCallbacks tickerMutex ticker updateInterval attatchments diedAt'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TreeModelAbstract subclass: #JavaClassHierarchyModel
	instanceVariableNames: 'classesRoot interfacesRoot arraysRoot primitivesRoot roots registry'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TreeModelAbstract subclass: #SupplementaryClassloaderTreeModel
	instanceVariableNames: 'jvm classloaders'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Presenter subclass: #JVMStatusPageAbstract
	instanceVariableNames: 'jvmStatus'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JVMStatusPageAbstract subclass: #JVMClassesPage
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'globalJavadocPath'!
JVMStatusPageAbstract subclass: #JVMClassloadersPage
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JVMStatusPageAbstract subclass: #JVMDBPrintfPage
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JVMStatusPageAbstract subclass: #JVMStatusPage
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
PolyViewerToolShell subclass: #JVMStatusShell
	instanceVariableNames: 'alphaBlend'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!JavaArrayClassStatic methodsFor!

browserString
	"answer the location of the JavaDoc for the receiver, this is
	taken relative to the JavaDoc root.  NB: this should only be
	called if #isBrowsable answers true"

	^ self elementClass browserString.!

canBeBrowsed
	"answer whether the receiver stands for a Java class which should be browsable"

	^ self elementClass canBeBrowsed.!

canBeWrapped
	"answer whether the receiver is wrappable"

	^ false.! !
!JavaArrayClassStatic categoriesFor: #browserString!browsing!public! !
!JavaArrayClassStatic categoriesFor: #canBeBrowsed!browsing!public!testing! !
!JavaArrayClassStatic categoriesFor: #canBeWrapped!public!purging!testing! !

!JavaPrimitiveStatic methodsFor!

canBeBrowsed
	"answer whether the receiver stands for a Java class which should be browsable"

	^ false.!

canBeWrapped
	"answer whether the receiver is wrappable"

	^ false.! !
!JavaPrimitiveStatic categoriesFor: #canBeBrowsed!browsing!public!testing! !
!JavaPrimitiveStatic categoriesFor: #canBeWrapped!generating!public!testing! !

!JavaStatic methodsFor!

browserString
	"answer the location of the JavaDoc for the receiver, this is
	taken relative to the JavaDoc root.  NB: this should only be
	called if #isBrowsable answers true"

	^ ((self name copyReplacing: $. withObject: $\)
		copyReplacing: $$ withObject: $.)
			, '.html'.!

canBeBrowsed
	"answer whether the receiver stands for a Java class which should be browsable"

	"default is that we can"
	^ true.!

canBeWrapped
	"answer whether the receiver is wrappable"

	^ true.!

generatedInstanceSelectors
	"answer a Collection of all the generated java methods
	that our instances respond to.
	Note that this does not include superclass methods"

	self javaSuperclassDo: [:it | it instanceClass == instanceClass ifTrue: [^ #()]].

	^ instanceClass generatedSelectors.!

generatedSelectors
	"answer a Collection of all the generated java methods
	that we respond to.
	Note that this does not include superclass methods"

	self javaSuperclassDo: [:it | it class == self class ifTrue: [^ #()]].

	^ self class generatedSelectors.!

isFake
	"private -- answer whether we are one of the 'fake' JavaStatics used by the Classes status
	page"

	^ false.! !
!JavaStatic categoriesFor: #browserString!browsing!public! !
!JavaStatic categoriesFor: #canBeBrowsed!browsing!public!testing! !
!JavaStatic categoriesFor: #canBeWrapped!public!testing! !
!JavaStatic categoriesFor: #generatedInstanceSelectors!listing wrapper methods!public! !
!JavaStatic categoriesFor: #generatedSelectors!listing wrapper methods!public! !
!JavaStatic categoriesFor: #isFake!public!testing! !

!JVMSettings methodsFor!

statusMonitorSettings
	"answer the subcollection of settings for the status monitor"

	^ self subSettings: #statusMonitorSettings.! !
!JVMSettings categoriesFor: #statusMonitorSettings!accessing!public! !

!SupplementaryClassloader methodsFor!

icon

	#CUtodo.  "this is just temporary"

	self isDisabled ifTrue: [^ Signal icon "red"].
	self isActive ifTrue:  [^ Presenter icon "green"].
	^ super icon.  "amber (ish)"! !
!SupplementaryClassloader categoriesFor: #icon!accessing!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

FakeJavaStatic guid: (GUID fromString: '{B828FDED-B7D5-46CE-A581-0FF70836CFA2}')!
FakeJavaStatic comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org

Used internally by the Java class tree presenter'!
!FakeJavaStatic categoriesForClass!Unclassified! !
!FakeJavaStatic methodsFor!

canBeBrowsed

	^ false.!

canBePurged

	^ false.!

canBeWrapped

	^ false.!

description

	^ description.
!

description: aString

	description := aString.
!

displayOn: aStream
	"append a user-centric description of ourselves to aStream"

	aStream nextPutAll: name.!

generatedInstanceSelectors

	^ #().!

generatedSelectors

	^ #().!

inheritanceString

	^ ''.!

inheritanceStringWithStatus

	^ ''.!

isFake

	^ true.!

isUnresolved

	^ false.
!

name

	^ name.
!

name: aString

	name := aString.
!

printOn: aStream
	"append a developer-centric description of ourself to aStream"

	aStream
		basicPrint: self;
		nextPutAll: ' name: ';
		nextPutAll: name.! !
!FakeJavaStatic categoriesFor: #canBeBrowsed!browsing!public!testing! !
!FakeJavaStatic categoriesFor: #canBePurged!public!purging!testing! !
!FakeJavaStatic categoriesFor: #canBeWrapped!generating!public!testing! !
!FakeJavaStatic categoriesFor: #description!accessing!public! !
!FakeJavaStatic categoriesFor: #description:!accessing!private! !
!FakeJavaStatic categoriesFor: #displayOn:!displaying!public! !
!FakeJavaStatic categoriesFor: #generatedInstanceSelectors!accessing!listing wrapper methods!public! !
!FakeJavaStatic categoriesFor: #generatedSelectors!accessing!listing wrapper methods!public! !
!FakeJavaStatic categoriesFor: #inheritanceString!displaying!public! !
!FakeJavaStatic categoriesFor: #inheritanceStringWithStatus!displaying!public! !
!FakeJavaStatic categoriesFor: #isFake!public!testing! !
!FakeJavaStatic categoriesFor: #isUnresolved!public! !
!FakeJavaStatic categoriesFor: #name!accessing!public! !
!FakeJavaStatic categoriesFor: #name:!accessing!private! !
!FakeJavaStatic categoriesFor: #printOn:!printing!public! !

!FakeJavaStatic class methodsFor!

icon
	"answer an Icon representing the receiver"

	^ JVM icon: 'JavaObject'.!

name: aString
	"answer a new instance which responds to #name with aString"

	^ (self new)
		name: aString;
		yourself.!

name: aString description: anotherString
	"answer a new instance which responds to #name with aString and #description with anotherString"

	^ (self new)
		name: aString;
		description: anotherString;
		yourself.! !
!FakeJavaStatic class categoriesFor: #icon!constants!ghost classes!public! !
!FakeJavaStatic class categoriesFor: #name:!instance creation!public! !
!FakeJavaStatic class categoriesFor: #name:description:!instance creation!public! !

JVMHistoryItem guid: (GUID fromString: '{C8E6D8CC-B82E-4F8A-9DBC-63F11C806697}')!
JVMHistoryItem comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JVMHistoryItem categoriesForClass!Unclassified! !
!JVMHistoryItem methodsFor!

births
	"answer how many births there were in the time we cover"

	^ births.
!

births: aNumber
	"private -- set the receiver's births to aNumber"

	births := aNumber.
!

birthsPerSecond
	"answer the Float rate of births per second during the interval we represent.
	(0 for invalid instances)"

	^ births asFloat / duration.!

callbacks
	"answer how many callbacks there were in the time we cover"

	^ callbacks.
!

callbacks: aNumber
	"private -- set the receiver's callbacks to aNumber"

	callbacks := aNumber.
!

callbacksPerSecond
	"answer the Float rate of callbacks per second during the interval we represent.
	(0 for invalid instances)"

	^ callbacks asFloat / duration.!

deaths
	"answer how many deaths there were in the time we cover"

	^ deaths.
!

deaths: aNumber
	"private -- set the receiver's deaths to aNumber"

	deaths := aNumber.
!

deathsPerSecond
	"answer the Float rate of births per second during the interval we represent.
	(0 for invalid instances)"

	^ deaths asFloat / duration.!

duration
	"answer how long the time we cover is (in seconds)"

	^ duration.
!

duration: aNumberOfSeconds
	"private -- set the receiver's duration to aNumberOfSeconds"

	duration := aNumberOfSeconds.
!

globalRefs
	"answer how many global references were in existence at the end of the time we cover"

	^ globalRefs.
!

globalRefs: aNumber
	"private -- set the receiver's globalRefs to aNumber"

	globalRefs := aNumber.
!

isValid
	"answer whether we represent real data"

	^ duration > 0.!

localRefs
	"answer how many local references were in existence at the end of the time we cover"

	^ localRefs.
!

localRefs: aNumber
	"private -- set the receiver's localRefs to aNumber"

	localRefs := aNumber.
!

max
	"answer the Float max of our population, birthsPerSecond and deathsPerSecond"

	^ (localRefs+globalRefs) asFloat max: ((births max: deaths) max: callbacks) / duration asFloat.!

objectRefs
	"answer how many object references were in existence at the end of the time we cover"

	^ localRefs + globalRefs.! !
!JVMHistoryItem categoriesFor: #births!accessing!public! !
!JVMHistoryItem categoriesFor: #births:!initializing!private! !
!JVMHistoryItem categoriesFor: #birthsPerSecond!accessing!public! !
!JVMHistoryItem categoriesFor: #callbacks!accessing!public! !
!JVMHistoryItem categoriesFor: #callbacks:!initializing!private! !
!JVMHistoryItem categoriesFor: #callbacksPerSecond!accessing!public! !
!JVMHistoryItem categoriesFor: #deaths!accessing!public! !
!JVMHistoryItem categoriesFor: #deaths:!initializing!private! !
!JVMHistoryItem categoriesFor: #deathsPerSecond!accessing!public! !
!JVMHistoryItem categoriesFor: #duration!accessing!public! !
!JVMHistoryItem categoriesFor: #duration:!initializing!private! !
!JVMHistoryItem categoriesFor: #globalRefs!accessing!public! !
!JVMHistoryItem categoriesFor: #globalRefs:!initializing!private! !
!JVMHistoryItem categoriesFor: #isValid!public!testing! !
!JVMHistoryItem categoriesFor: #localRefs!accessing!public! !
!JVMHistoryItem categoriesFor: #localRefs:!initializing!private! !
!JVMHistoryItem categoriesFor: #max!accessing!public! !
!JVMHistoryItem categoriesFor: #objectRefs!accessing!public! !

!JVMHistoryItem class methodsFor!

initialize
	"private -- class initialisation.

		self initialize.
	"

	noData := (self new)
			localRefs: 0;
			globalRefs: 0;
			births: 0;
			deaths: 0;
			callbacks: 0;
			duration: -1;
			yourself.!

noData
	"answer the single items which is used specially to represent an absense of data"

	^ noData.!

uninitialize
	"private -- class teardown.

		self uninitialize.
	"

	noData := nil.! !
!JVMHistoryItem class categoriesFor: #initialize!initializing!private! !
!JVMHistoryItem class categoriesFor: #noData!accessing!public! !
!JVMHistoryItem class categoriesFor: #uninitialize!initializing!private! !

JVMStatusMonitorSettings guid: (GUID fromString: '{82B0EA54-A0B7-4B36-A565-22CB5561C118}')!
JVMStatusMonitorSettings comment: 'Copyright © Chris Uppal, 2005.
chris.uppal@metagnostic.org

One of these holds settings that are only used by the JVMStatusMonitor tool.'!
!JVMStatusMonitorSettings categoriesForClass!Unclassified! !
!JVMStatusMonitorSettings methodsFor!

defaultSampleSize
	"answer our default sample size.
	This is the 'sample size' that a status mononitor watching aJVM with these options
	will use initially"

	^ defaultSampleSize.
!

defaultSampleSize: anInteger
	"set our default sample size to anInteger"

	defaultSampleSize := anInteger.!

defaultUpdateInterval
	"answer our default update interval.
	This is the 'update interval' that a status mononitor watching aJVM with these options
	will use initially"

	^ defaultUpdateInterval.
!

defaultUpdateInterval: anInteger
	"set our default update interval to anInteger"

	defaultUpdateInterval := anInteger.!

initialize
	"private -- establish a default initial state.
	NB: this is only used for creating a new 'default' (template) instance.  Subsequent
	instances are created by cloning that"

	javadocPath := ''.
	defaultUpdateInterval := JVMStatusModel defaultUpdateInterval.
	defaultSampleSize := JVMStatusModel defaultSampleSize.
	super initialize.!

javadocPath
	"answer our javadoc path.
	This is a ';'-separated list of directory names that a status mononitor watching aJVM will
	append to its global 'javadoc path'.  The path is searched for javadoc documentation"

	^ javadocPath.
!

javadocPath: aString
	"private -- set our javadoc path to aString"

	javadocPath := aString.! !
!JVMStatusMonitorSettings categoriesFor: #defaultSampleSize!accessing!public! !
!JVMStatusMonitorSettings categoriesFor: #defaultSampleSize:!accessing!public! !
!JVMStatusMonitorSettings categoriesFor: #defaultUpdateInterval!accessing!public! !
!JVMStatusMonitorSettings categoriesFor: #defaultUpdateInterval:!accessing!public! !
!JVMStatusMonitorSettings categoriesFor: #initialize!initializing!private! !
!JVMStatusMonitorSettings categoriesFor: #javadocPath!accessing!public! !
!JVMStatusMonitorSettings categoriesFor: #javadocPath:!accessing!private! !

!JVMStatusMonitorSettings class methodsFor!

initialize
	"private -- class initialization.

		self initialize.
	"

	JVMSettings addToTemplate: self new name: #statusMonitorSettings.

!

integerAspectNames
	"answer an Array of the names of integer aspects of instances"

	^ super stringAspectNames
		, #(
			#defaultSampleSize
			#defaultUpdateInterval
		).!

stringAspectNames
	"answer an Array of the names of string aspects of instances"

	^ super stringAspectNames
		, #(
			#javadocPath
		).!

uninitialize
	"private -- class-side tear-down.

		self uninitialize.
	"

	JVMSettings removeFromTemplate: #statusMonitorSettings.
! !
!JVMStatusMonitorSettings class categoriesFor: #initialize!initializing!private! !
!JVMStatusMonitorSettings class categoriesFor: #integerAspectNames!constants!development!must strip!private! !
!JVMStatusMonitorSettings class categoriesFor: #stringAspectNames!constants!development!must strip!private! !
!JVMStatusMonitorSettings class categoriesFor: #uninitialize!initializing!private! !

JVMStatusModel guid: (GUID fromString: '{63227CF7-CC50-44C8-BBD2-C1CA4C814960}')!
JVMStatusModel comment: 'Copyright © Chris Uppal, 2002 - 2005.
chris.uppal@metagnostic.org

Intermediate model for the JVMStatusMonitor.  This is the real model that it uses.  This object collects data about one running JVM object, and acts as a facade for it.

Note: originally this was meant to be a complete facade -- so that the Status Monitor never knew about the real JVM at all -- but that proved unweildy, and I''ve tended to abandon the idea.  There''s now an uncomfortable, and rather arbitrary, mix of code styles: some data is accessed via the facade, some is not.  I ought to clean it up, but, what the hell, it''s only GUI stuff anyway...'!
!JVMStatusModel categoriesForClass!Unclassified! !
!JVMStatusModel methodsFor!

attatch
	"tells the model that another MVP triad is using it (we need to do this
	so we can clean up when the last view goes away)"

	attatchments := attatchments + 1.
	attatchments > 0 ifTrue: [self startTicker].
!

callbackRegistryStatusDefinition
	"private -- answer a collection of #(String Selector) pairs which define the status we retrieve from our JVM's
	callback registry on every tick"

	^ #(
		('Callbacks' #requestsCompleted)
		('Callback queue length' #queueLength)
		('Callback queue max' #maxQueueLength)
		('Callbacks registered' #tableSize)
"		('Callbacks table load (percent)' #tableLoad)
		('Callbacks table APE' #averageProbesPerElement)
"
	).!

canShutdownJVM
	"answer whether we represent a JVM that can be shutdown"

	^ self jvmIsLive.!

classesTreeModel
	"answer a JavaClassHierarchyModel for our JVM's class registry"

	^ self jvmIsLive
		ifTrue: [JavaClassHierarchyModel on: jvm]
		ifFalse: [TreeModel new].
!

classIndexStatusDefinition
	"private -- answer a collection of #(String Selector) pairs which define the status we retrieve from our JVM's
	class index on every tick"

	^ #(
		('Class index size' #tableSize)
"		('Class index load (percent)' #tableLoad)
		('Class index APE' #averageProbesPerElement)
"	).!

classRegistryStatusDefinition
	"private -- answer a collection of #(String Selector) pairs which define the status we retrieve from our JVM's
	class registry on every tick"

	^ #(
		('Class registry size' #tableSize)
"		('Class registry load (percent)' #tableLoad)
		('Class registry APE' #averageProbesPerElement)
"	).!

defer: a0Block
	"private -- evaluate a0Block in the UI thread"

	SessionManager inputState queueDeferredAction: a0Block.
!

deferUpdateAnd: a0Block
	"private -- update our status then evaluate a0Block in the UI thread"

	self defer: [self updateStatus. a0Block value].
!

detatch
	"tells the model that a MVP triad is going away.  We need to do this
	so we can stop the monitor thread when the last real user goes away,
	we can't use finalisation because the thread would keep us alive
	indefinately)"

	attatchments := attatchments - 1.
	attatchments < 1 ifTrue: [self stopTicker].!

diedAt
	"answer an array [date time] telling when the JVM died, or nil if it hasn't died"

	^ diedAt copy.!

displayOn: aStream
	"append a user-centric description of ourself to aStream"

	aStream nextPutAll: (jvm isNil ifTrue: ['<none>'] ifFalse: [jvm name]).!

history
	"answer the RollingAccumulator that is the receiver's history"

	^ history.
!

initialize
	"private -- establish a coherent initial state"

	tickerMutex := Mutex new.
	attatchments := 0.
	statusList := ListModel on: (OrderedCollection new).
	self sampleSize: self class defaultSampleSize.
	self updateInterval: self class defaultUpdateInterval.
!

jniHelperStatusDefinition
	"private -- answer a collection of #(String Selector) pairs which define the status we retrieve from our JVM's
	jniHelper on every tick"

	^ #(
		('Hook queue length' #queueLength)
		('Hook queue max' #maxQueueLength)
	).!

jvm
	"answer the JVM we target"

	^ jvm.!

jvm: aJVM
	"private -- set the JVM we monitor"

	| settings |

	jvm := aJVM.

	jvm
		when: #JVMIsBorn send: #onJVMIsBorn to: self;
		when: #JVMIsLive send: #onJVMIsLive to: self;
		when: #JVMIsDead send: #onJVMIsDead to: self;
		when: #JVMShutdownStarting send: #onJVMShutdownStarting to: self;
		when: #JVMAbort send: #onJVMAbort to: self;
		when: #JVMExit: send: #onJVMExit: to: self;
		when: #JVMMessage:address: send: #onJVMMessage:address: to: self;
		when: #JVMSupplementaryClassloadersChanged send: #onJVMSupplementaryClassloadersChanged to: self;
		when: #JVMReset send: #onJVMReset to: self.

	"if it's already live than we can't wait for the #isLive signal to start tracking the
	registry"
	jvm isLive ifTrue:
		[jvm classRegistry
			when: #classRegistered: send: #onClassRegistered: to: self;
			when: #classPurged: send: #onClassPurged: to: self].

	settings := aJVM settings statusMonitorSettings.
	self
		sampleSize: settings defaultSampleSize;
		updateInterval: settings defaultUpdateInterval;	
		updateStatus.
!

jvmIsLive
	"answer whether we represent a live JVM"

	^ jvm notNil and: [jvm isLive].!

jvmIsRunning
	"answer whether we represent a live JVM that is fully initialised"

	^ jvm notNil and: [jvm isRunning].
!

jvmStatusDefinition
	"private -- answer a collection of #(String Selector) pairs which define the status we retrieve from our JVM
	on every tick"

	^ #(
		('JVM name' #name)
		('Status' #status)
		('Ghost classes' #ghostClassMode)
		('Supports callbacks' #supportsCallbacks)
		('Supplementary classloaders' #usingSupplementaryClassloaders)
		('Local object refs' #localRefCount)
		('Global object refs' #globalRefCount)
		('Total object refs' #objectRefCount)
		('Locals pool allocated' #localsPoolCapacity)
		('Objects created' #objectsCreated)
		('Objects released' #objectsReleased)
		('Callback nesting' #callbackDepth)
	).!

jvmSupportsCallbacks
	"answer whether the JVM we wrap is able to support Java callbacks"

	^ self jvmIsLive and: [jvm supportsCallbacks].!

onClassPurged: aJavaStatic
	"private -- a Java class object has been purged"

	self deferUpdateAnd: [self jvmIsLive ifTrue: [self trigger: #classPurged: with: aJavaStatic]].!

onClassRegistered: aJavaStatic
	"private -- a new Java class object has been registered"

	self deferUpdateAnd: [self jvmIsLive ifTrue: [self trigger: #classRegistered: with: aJavaStatic]].!

oneTick
	"private -- resample our target JVM and update our status and history lists accordingly,
	this is called from the ticker thread"

	"sample the history right now, this will send #historyChanged notification
	immediately (not deferred, and hence not from the UI thread)"
	self updateHistory.

	"but there is no point in updating the dynamic status until someone is going to read it,
	so we defer that update"
	self defer: [self updateStatus].!

onJVMAbort
	"private -- the JVM has aborted itself (and -- suprisingly -- hasn't taken Dolphin out with it!!)"

	self deferUpdateAnd: [self trigger: #JVMAbort].
!

onJVMExit: aNumber
	"private -- the JVM has exited itself (and -- suprisingly -- hasn't taken Dolphin out with it!!)"

	self deferUpdateAnd: [self trigger: #JVMExit: with: aNumber].!

onJVMIsBorn
	"private -- the JVM is now officially born"

	self deferUpdateAnd: [self trigger: #JVMIsBorn].!

onJVMIsDead
	"private -- the JVM is now dead"

	diedAt := Array with: Date new with: Time now.
	self deferUpdateAnd: [self trigger: #JVMIsDead].!

onJVMIsLive
	"private -- the JVM is now live"

	jvm classRegistry
			when: #classRegistered: send: #onClassRegistered: to: self;
			when: #classPurged: send: #onClassPurged: to: self.

	self deferUpdateAnd: [self trigger: #JVMIsLive].!

onJVMMessage: aString address: anExternalAddress
	"private -- the JVM has attempted to vprintf() aString to anExternalAddress"

	self defer: [self trigger: #JVMMessage:address: with: aString with: anExternalAddress].!

onJVMReset
	"private -- the JVM has been reset"

	self deferUpdateAnd: [self trigger: #JVMReset].!

onJVMShutdownStarting
	"private -- the JVM is about to start a shutdown process"

	self deferUpdateAnd: [self trigger: #JVMShutdownStarting].!

onJVMSupplementaryClassloadersChanged
	"private -- the JVM's use of SupplementaryClassloaders has changed"

	self deferUpdateAnd: [self trigger: #JVMSupplementaryClassloadersChanged].!

printOn: aStream
	"append a developer-centric description of ourself to aStream"

	aStream
		basicPrint: self;
		nextPut: $(;
		display: self;
		nextPut: $).!

sampleSize
	"answer how many samples we keep"

	^ history isNil ifTrue: [0] ifFalse: [history size].
!

sampleSize: anInteger
	"set the number of samples we keep"

	| noData |

	noData := JVMHistoryItem noData.

	history isNil
		ifTrue: [history := RollingAccumulator new: anInteger withAll: noData]
		ifFalse: [history resize: anInteger withAll: noData].!

shutdownJVM
	"shutdown the JVM we stand for"

	self canShutdownJVM ifTrue: [jvm shutdown].
!

startTicker
	"private -- ensure that we have a ticker running if we need one"

	tickerMutex critical:
		[ticker isNil ifTrue:
			[ticker := self tickerProcess.
			ticker resume]].
!

statusAt: aString put: aValue
	"private -- update/add the given status element"

	| item |

	item := aString -> aValue.
	statusList keysAndValuesDo: [:index :each | each key = aString ifTrue: [statusList at: index put: item. ^self]].
	statusList addLast: item.
!

statusList
	"answer a ListModel of (name->value) pairs for the various aspects of our
	JVM's status"

	^ statusList.
!

stopTicker
	"private -- stop any active monitoring of the JVM"

	"if there's a ticker running, it'll see this and stop"
	tickerMutex critical: [ticker := nil].!

supplementaryClassloadersTreeModel
	"answer a SupplementaryClassloaderTreeModel for our JVM's classloaders"

	^ (jvm isNil)
		ifTrue: [TreeModel new]
		ifFalse: [SupplementaryClassloaderTreeModel on: jvm].
!

tickerLoop
	"private -- keep updating ourself for as long as the taget is live, the update interval is
	set, and we are in the intended ticker process"

	| running interval |

	running := Processor activeProcess.

	[| ok |
	ok := tickerMutex critical: [(running == ticker) and: [(interval := updateInterval) >= 1]].
	ok and: [jvm notNil and: [jvm isLive]]] whileTrue:
		[self oneTick.
		(Delay forSeconds: interval) wait].

	tickerMutex critical: [ticker := nil].
!

tickerProcess
	"private -- answer a Process (not yet running) which will cause ourselves to update
	for as long as it is our ticker process"

	lastObjectsCreated := jvm ifNil: [0] ifNotNil: [:it | it objectsCreated].
	lastObjectsReleased := jvm ifNil: [0] ifNotNil: [:it | it objectsReleased].
	lastCallbacks := jvm ifNil: [0] ifNotNil: [:it | it callbacks].

	^ ([self tickerLoop] newProcess)
		priority: Processor activeProcess priority + 1;	"NB: higher priority for the ticker"
		name: ('Monitoring ' , jvm name);
		yourself.

	!

updateHistory
	"private -- update our history lists from the JVM"

	| item objectsCreated objectsReleased callbacks localRefs globalRefs births deaths |

	objectsCreated := jvm objectsCreated.
	objectsReleased := jvm objectsReleased.
	callbacks := jvm callbacks.

	item := (JVMHistoryItem new)
			localRefs: jvm localRefCount;
			globalRefs: jvm globalRefCount;
			births: objectsCreated - lastObjectsCreated;
			deaths: objectsReleased - lastObjectsReleased;
			callbacks: callbacks - lastCallbacks;
			duration: updateInterval;
			yourself.
	history add: item.

	lastObjectsCreated := objectsCreated.
	lastObjectsReleased := objectsReleased.
	lastCallbacks := callbacks.

	"NB: this notification is *NOT* defered, it is the responsibility of the Observer to
	ensure that handling is appropriately deferred (if necessary)"
	self trigger: #historyChanged.
!

updateInterval
	"answer how often the receiver updates itself (in seconds), 0 if we aren't running"

	^ updateInterval.
!

updateInterval: aNumberOfSeconds
	"set the receiver's updateInterval to aNumberOfSeconds, this
	will start/stop the ticker process as necessary"

	tickerMutex critical: [updateInterval := aNumberOfSeconds].
	updateInterval >= 1 ifTrue: [self startTicker].!

updateStatus
	"private -- update our statusList from the JVM"

	self
		updateStatusItems: self jvmStatusDefinition from: jvm;
		updateStatusItems: self classRegistryStatusDefinition from: jvm classRegistry;
		updateStatusItems: self classIndexStatusDefinition from: jvm classIndex;
		updateStatusItems: self callbackRegistryStatusDefinition from: jvm callbackRegistry;
		updateStatusItems: self jniHelperStatusDefinition from: jvm jniHelper;
		yourself.!

updateStatusItems: aCollection from: anObject
	"private -- given a collection of #(String Selector) pairs, update the status item named by
	each string with the result of sendng the selector to anObject"

	anObject isNil ifTrue: [^ self].

	aCollection do:
		[:each || value |
		value := [anObject perform: each second] on: Error do: [:e | '<<unknown>>'].
		self statusAt: each first put: value].
!

wrapperGeneratorSettings
	"answer the wrapperGeneratorSettings of our underlying JVM"

	^ jvm settings wrapperGeneratorSettings.! !
!JVMStatusModel categoriesFor: #attatch!initialize/release!public! !
!JVMStatusModel categoriesFor: #callbackRegistryStatusDefinition!constants!private! !
!JVMStatusModel categoriesFor: #canShutdownJVM!public!testing! !
!JVMStatusModel categoriesFor: #classesTreeModel!accessing!public! !
!JVMStatusModel categoriesFor: #classIndexStatusDefinition!constants!private! !
!JVMStatusModel categoriesFor: #classRegistryStatusDefinition!constants!private! !
!JVMStatusModel categoriesFor: #defer:!helpers!private! !
!JVMStatusModel categoriesFor: #deferUpdateAnd:!helpers!private! !
!JVMStatusModel categoriesFor: #detatch!initialize/release!public! !
!JVMStatusModel categoriesFor: #diedAt!accessing!public! !
!JVMStatusModel categoriesFor: #displayOn:!displaying!public! !
!JVMStatusModel categoriesFor: #history!accessing!public! !
!JVMStatusModel categoriesFor: #initialize!initializing!private! !
!JVMStatusModel categoriesFor: #jniHelperStatusDefinition!constants!private! !
!JVMStatusModel categoriesFor: #jvm!accessing!public! !
!JVMStatusModel categoriesFor: #jvm:!event handling!initializing!private! !
!JVMStatusModel categoriesFor: #jvmIsLive!public!testing! !
!JVMStatusModel categoriesFor: #jvmIsRunning!public!testing! !
!JVMStatusModel categoriesFor: #jvmStatusDefinition!constants!private! !
!JVMStatusModel categoriesFor: #jvmSupportsCallbacks!public!testing! !
!JVMStatusModel categoriesFor: #onClassPurged:!event handling!private! !
!JVMStatusModel categoriesFor: #onClassRegistered:!event handling!private! !
!JVMStatusModel categoriesFor: #oneTick!private!updating! !
!JVMStatusModel categoriesFor: #onJVMAbort!event handling!private! !
!JVMStatusModel categoriesFor: #onJVMExit:!event handling!private! !
!JVMStatusModel categoriesFor: #onJVMIsBorn!event handling!private! !
!JVMStatusModel categoriesFor: #onJVMIsDead!event handling!private! !
!JVMStatusModel categoriesFor: #onJVMIsLive!event handling!private! !
!JVMStatusModel categoriesFor: #onJVMMessage:address:!event handling!private! !
!JVMStatusModel categoriesFor: #onJVMReset!event handling!private! !
!JVMStatusModel categoriesFor: #onJVMShutdownStarting!event handling!private! !
!JVMStatusModel categoriesFor: #onJVMSupplementaryClassloadersChanged!event handling!private! !
!JVMStatusModel categoriesFor: #printOn:!printing!public! !
!JVMStatusModel categoriesFor: #sampleSize!accessing!public! !
!JVMStatusModel categoriesFor: #sampleSize:!accessing!public! !
!JVMStatusModel categoriesFor: #shutdownJVM!operations!public! !
!JVMStatusModel categoriesFor: #startTicker!accessing!private!updating! !
!JVMStatusModel categoriesFor: #statusAt:put:!accessing!private! !
!JVMStatusModel categoriesFor: #statusList!accessing!public! !
!JVMStatusModel categoriesFor: #stopTicker!operations!private!updating! !
!JVMStatusModel categoriesFor: #supplementaryClassloadersTreeModel!accessing!public! !
!JVMStatusModel categoriesFor: #tickerLoop!private!updating! !
!JVMStatusModel categoriesFor: #tickerProcess!private!updating! !
!JVMStatusModel categoriesFor: #updateHistory!private!updating! !
!JVMStatusModel categoriesFor: #updateInterval!accessing!public! !
!JVMStatusModel categoriesFor: #updateInterval:!accessing!public!updating! !
!JVMStatusModel categoriesFor: #updateStatus!private!updating! !
!JVMStatusModel categoriesFor: #updateStatusItems:from:!private!updating! !
!JVMStatusModel categoriesFor: #wrapperGeneratorSettings!accessing!public! !

!JVMStatusModel class methodsFor!

default
	"answer an instance for a running JVM (selected arbitrarily if there's more than one)
	or a new dead one if none can be found"

	^ self for: (JVM currentIfNone: [^ self new]).!

defaultSampleSize
	"answer the default number of samples to keep"

	^ 256.!

defaultUpdateInterval
	"answer the default update interval to use"

	"no automatic updates"
	^ 0.!

for: aJVM
	"answer a (possibly new) instance that is attachted to the given JVM"

	^ self allSubinstances detect: [:each | each jvm == aJVM] ifNone: [(self new) jvm: aJVM; yourself].!

publishedEventsOfInstances
	"answer a Set of Symbols that describe the published events triggered
	by JVM instances"	

	"since our instances re-trigger all the JVM's events"
	^ (super publishedEventsOfInstances)
		addAll: JVM publishedEventsOfInstances;
		addAll: JavaClassRegistry publishedEventsOfInstances;
		yourself.! !
!JVMStatusModel class categoriesFor: #default!instance creation!public! !
!JVMStatusModel class categoriesFor: #defaultSampleSize!constants!public! !
!JVMStatusModel class categoriesFor: #defaultUpdateInterval!constants!public! !
!JVMStatusModel class categoriesFor: #for:!instance creation!public! !
!JVMStatusModel class categoriesFor: #publishedEventsOfInstances!development!events!public! !

JavaClassHierarchyModel guid: (GUID fromString: '{59D681D5-F495-4526-A4B5-05B7AC02A82F}')!
JavaClassHierarchyModel comment: 'Copyright © Chris Uppal, 2002 - 2005.
chris.uppal@metagnostic.org

Used by the ''Classes'' page of the JVM Statis Monitor.'!
!JavaClassHierarchyModel categoriesForClass!Unclassified! !
!JavaClassHierarchyModel methodsFor!

add: aNode asChildOf: aParentNode

	^self shouldNotImplement.!

childrenOf: aJavaStaticOrRoot
	"answer aJavaStaticOrRoot's children"

	"we don't use 
		asSortedCollection: (SortStringsAscending by: #name)
	because that uses #perform: which in turn causes lazy ghosts to
	resolve themselves"

	^ ((self rawChildrenOf: aJavaStaticOrRoot)
		asSortedCollection: [:e1 :e2 | (e1 name strcmp: e2 name) < 0])
			asArray.!

clear

	self shouldNotImplement.
!

hasChildren: aJavaStaticOrRoot
	"answer true iff aJavaStatic has children"

	registry isDead ifTrue: [^ false].

	^ (self isRoot: aJavaStaticOrRoot) or: [(registry knownDirectSubclassesOf: aJavaStaticOrRoot) notEmpty].
!

initialize
	"private -- establish a coherent initial state"

	classesRoot := FakeJavaStatic name: 'Classes' description: 'java.lang.Object and known subclasses excluding array types'.
	interfacesRoot := FakeJavaStatic name: 'Interfaces' description: 'Known interface types'.
	arraysRoot := FakeJavaStatic name: 'Array classes' description: 'Array types in use'.
	primitivesRoot := FakeJavaStatic name: 'Primitive types' description: 'Java primitive types'.

	roots := Array with: classesRoot with: interfacesRoot with: arraysRoot with: primitivesRoot.!

isRoot: aJavaStaticOrRoot
	"private -- answer whether aJavaStaticOrRoot is one of our 'virtual' (i.e. 'magic')
	root nodes"

	^ roots identityIncludes: aJavaStaticOrRoot.!

move: aNode asChildOf: aParentNode

	self shouldNotImplement.!

onClassPurged: aJavaStatic
	"private -- aJavaStatic has just been purged from the class registry"

	| child parent |

	child := aJavaStatic.
	parent := self parentForJavaClass: child.

	self trigger: #item:removedFromParent: with: child with: parent.
!

onClassRegistered: aJavaStatic
	"private -- aJavaStatic has just been added to the class registry"

	| child parent |

	child := aJavaStatic.
	parent := self parentForJavaClass: child.

	self trigger: #item:addedInParent: with: child with: parent.!

parentForJavaClass: aJavaStatic
	"private -- answer the appropriate parent for aJavaStatic"

	aJavaStatic isArray ifTrue: [^ arraysRoot].
	aJavaStatic isPrimitive ifTrue: [^ primitivesRoot].
	aJavaStatic isInterface ifTrue: [^ interfacesRoot].

	^ aJavaStatic javaSuperclassIfNone: [classesRoot].
  !

parentOf: aJavaStaticOrRoot
	"answer aJavaStatic's parent, which may be nil"

	^ (self isRoot: aJavaStaticOrRoot)
		ifTrue: [nil]
		ifFalse: [self parentForJavaClass: aJavaStaticOrRoot].!

rawChildrenOf: aJavaStaticOrRoot
	"private -- answer aJavaStaticOrRoot's children"

	registry isDead ifTrue: [^ #()].

	aJavaStaticOrRoot == classesRoot ifTrue: [^ registry allRootClasses].
	aJavaStaticOrRoot == interfacesRoot ifTrue: [^ registry allInterfaces].
	aJavaStaticOrRoot == arraysRoot ifTrue: [^ registry allArrayClasses].
	aJavaStaticOrRoot == primitivesRoot ifTrue: [^ registry allPrimitiveTypes].

	^ (registry knownDirectSubclassesOf: aJavaStaticOrRoot) reject: [:each | each isArray].!

registry: aJavaClassRegistry
	"private -- set the receiver's registry to anObject"

	registry := aJavaClassRegistry.
	registry
		when: #classRegistered: send: #onClassRegistered: to: self;
		when: #classPurged: send: #onClassPurged: to: self.
!

remove: aNode

	self shouldNotImplement.!

roots
	"answer the roots of the receiver's hierarchy"

	^ roots.!

roots: aCollectionOfNodes

	self shouldNotImplement.
! !
!JavaClassHierarchyModel categoriesFor: #add:asChildOf:!adding!public! !
!JavaClassHierarchyModel categoriesFor: #childrenOf:!hierarchy!public! !
!JavaClassHierarchyModel categoriesFor: #clear!public!removing! !
!JavaClassHierarchyModel categoriesFor: #hasChildren:!hierarchy!public!testing! !
!JavaClassHierarchyModel categoriesFor: #initialize!initializing!private! !
!JavaClassHierarchyModel categoriesFor: #isRoot:!private!testing! !
!JavaClassHierarchyModel categoriesFor: #move:asChildOf:!public!updating! !
!JavaClassHierarchyModel categoriesFor: #onClassPurged:!event handling!private! !
!JavaClassHierarchyModel categoriesFor: #onClassRegistered:!event handling!private! !
!JavaClassHierarchyModel categoriesFor: #parentForJavaClass:!hierarchy!private! !
!JavaClassHierarchyModel categoriesFor: #parentOf:!hierarchy!public! !
!JavaClassHierarchyModel categoriesFor: #rawChildrenOf:!hierarchy!private! !
!JavaClassHierarchyModel categoriesFor: #registry:!initializing!private! !
!JavaClassHierarchyModel categoriesFor: #remove:!public!removing! !
!JavaClassHierarchyModel categoriesFor: #roots!accessing!hierarchy!public! !
!JavaClassHierarchyModel categoriesFor: #roots:!accessing!public! !

!JavaClassHierarchyModel class methodsFor!

on: aJVM
	"answer a new instance which exposes a changing view of the hierachy of Java classes loaded
	into aJVM"

	^ (self new)
		registry: aJVM classRegistry;
		yourself.! !
!JavaClassHierarchyModel class categoriesFor: #on:!instance creation!public! !

SupplementaryClassloaderTreeModel guid: (GUID fromString: '{40F78A5C-63FD-4975-884B-A3F55D236319}')!
SupplementaryClassloaderTreeModel comment: 'Copyright © Chris Uppal, 2005.
chris.uppal@metagnostic.org

Used by the ''Classloaders'' page of the JVM Statis Monitor.'!
!SupplementaryClassloaderTreeModel categoriesForClass!Unclassified! !
!SupplementaryClassloaderTreeModel methodsFor!

add: aNode asChildOf: aParentNode

	^self shouldNotImplement.!

childrenOf: aSupplementaryClassloader
	"answer aSupplementaryClassloader children"

	^ self sort: (self rawChildrenOf: aSupplementaryClassloader).
!

clear

	self shouldNotImplement.
!

declineEvents
	"private -- stop watching for events from our owning JVM"

	jvm removeEventsTriggeredFor: self.
	classloaders removeEventsTriggeredFor: self.!

hasChildren: aSupplementaryClassloader
	"answer true iff aSupplementaryClassloader has children"

	^ classloaders
		ifNil: [false]
		ifNotNil: [:it | it entryHasChildren: aSupplementaryClassloader].
!

jvm: aJVM
	"private -- set our owning JVM"

	self declineEvents.
	jvm := aJVM.
	self solicitEvents.
!

move: aNode asChildOf: aParentNode

	self shouldNotImplement.!

onClassloaderAdded: aSupplementaryClassloader
	"private -- the definition of a classloader has changed, update the tree accordingly"

	#CUtodo.  "implement this properly (and deferred)"
	self trigger: #treeChanged: with: nil.

!

onClassloaderChanged: aSupplementaryClassloader
	"private -- a new classloader has been added to the tree, update accordingly"

	#CUtodo.  "implement this properly (and deferred)"
	self trigger: #treeChanged: with: nil.

!

onClassloaderRemoved: aSupplementaryClassloader
	"private -- a classloader has been removed from the tree, update accordingly"

	#CUtodo.  "implement this properly (and deferred)"
	self trigger: #treeChanged: with: nil.

!

onJVMSupplementaryClassloadersChanged
	"private -- our owning JVM's classloader tree has been replaced"

	classloaders removeEventsTriggeredFor: self.
	classloaders := jvm supplementaryClassloaders.
	classloaders notNil ifTrue: [classloaders
						when: #supplementaryClassloaderAdded: send: #onClassloaderAdded: to: self;
						when: #supplementaryClassloaderChanged: send: #onClassloaderChanged: to: self;
						when: #supplementaryClassloaderRemoved: send: #onClassloaderRemoved: to: self].

	self trigger: #treeChanged: with: nil.
!

parentOf: aSupplementaryClassloader
	"answer aSupplementaryClassloader's parent, which may be nil"

	^ aSupplementaryClassloader parentEntry.
!

rawChildrenOf: aSupplementaryClassloader
	"private -- answer aSupplementaryClassloader'schildren"

	^ classloaders
		ifNil: [#()]
		ifNotNil: [:it | it childrenOfEntry: aSupplementaryClassloader].!

rawRoots
	"private -- answer the roots of the receiver's hierarchy"

	^ classloaders
		ifNil: [#()]
		ifNotNil: [:it | it rootEntries].!

remove: aNode

	self shouldNotImplement.!

roots
	"answer the roots of the receiver's hierarchy"

	^ self sort: self rawRoots.!

roots: aCollectionOfNodes

	self shouldNotImplement.
!

solicitEvents
	"private -- start watching for events from our owning JVM"

	jvm when: #JVMSupplementaryClassloadersChanged send: #onJVMSupplementaryClassloadersChanged to: self.
	self onJVMSupplementaryClassloadersChanged.!

sort: aCollection
	"private -- answer the given collection of classloaders in sorted order"

	^ (aCollection asSortedCollection: (SortStringsAscending by: #name))
			asArray.
! !
!SupplementaryClassloaderTreeModel categoriesFor: #add:asChildOf:!adding!public! !
!SupplementaryClassloaderTreeModel categoriesFor: #childrenOf:!hierarchy!public! !
!SupplementaryClassloaderTreeModel categoriesFor: #clear!public!removing! !
!SupplementaryClassloaderTreeModel categoriesFor: #declineEvents!event handling!private! !
!SupplementaryClassloaderTreeModel categoriesFor: #hasChildren:!hierarchy!public!testing! !
!SupplementaryClassloaderTreeModel categoriesFor: #jvm:!initializing!private! !
!SupplementaryClassloaderTreeModel categoriesFor: #move:asChildOf:!public!updating! !
!SupplementaryClassloaderTreeModel categoriesFor: #onClassloaderAdded:!event handling!private! !
!SupplementaryClassloaderTreeModel categoriesFor: #onClassloaderChanged:!event handling!private! !
!SupplementaryClassloaderTreeModel categoriesFor: #onClassloaderRemoved:!event handling!private! !
!SupplementaryClassloaderTreeModel categoriesFor: #onJVMSupplementaryClassloadersChanged!event handling!private! !
!SupplementaryClassloaderTreeModel categoriesFor: #parentOf:!hierarchy!public! !
!SupplementaryClassloaderTreeModel categoriesFor: #rawChildrenOf:!hierarchy!private! !
!SupplementaryClassloaderTreeModel categoriesFor: #rawRoots!hierarchy!private! !
!SupplementaryClassloaderTreeModel categoriesFor: #remove:!public!removing! !
!SupplementaryClassloaderTreeModel categoriesFor: #roots!hierarchy!public! !
!SupplementaryClassloaderTreeModel categoriesFor: #roots:!accessing!public! !
!SupplementaryClassloaderTreeModel categoriesFor: #solicitEvents!event handling!private! !
!SupplementaryClassloaderTreeModel categoriesFor: #sort:!private! !

!SupplementaryClassloaderTreeModel class methodsFor!

on: aJVM
	"answer a new instance which exposes a changing view of the hierachy of supplementary classloaders
	used by aJVM"

	^ (self new)
		jvm: aJVM;
		yourself.! !
!SupplementaryClassloaderTreeModel class categoriesFor: #on:!instance creation!public! !

JVMStatusPageAbstract guid: (GUID fromString: '{01FE231B-C39B-46D4-8B10-5BCD192A7FDB}')!
JVMStatusPageAbstract comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JVMStatusPageAbstract categoriesForClass!Unclassified! !
!JVMStatusPageAbstract methodsFor!

help
	"command -- display the help for this page"

	| locator file |

	"this will have to be recompiled if the package we live in changes its name"
	locator := PackageRelativeFileLocator packageNamed: ##(self owningPackage name).
	file := locator localFileSpecFor: ('Docs\JNIPort\' , self helpFileName).
	[ShellLibrary default shellOpen: file]
		on: Error
		do: [:err | MessageBox
			notify: ('Sorry.  No help for the ' , self class name , ' is available')
			caption: 'JVM Status Help'].

!

helpFileName
	"private -- answer the filename of our help text; this
	is relative to the documentation directory"

	^ 'status-monitor.html'.!

initialize
	"private -- establish a coherent initial state"

	jvmStatus := ''.

	super initialize.
!

jvmStatus: aString
	"set the text we dislay in our status line"

	jvmStatus := aString.
	self trigger: #polyViewerStatusChanged.
!

model: aJVMStatusModel
	"private -- set the model for this Presenter"

	self model notNil ifTrue: [self model removeEventsTriggeredFor: self].

	super model: aJVMStatusModel.

	self model
		when: #JVMIsBorn send: #onJVMIsBorn to: self;
		when: #JVMIsLive send: #onJVMIsLive to: self;
		when: #JVMIsDead send: #onJVMIsDead to: self;
		when: #JVMShutdownStarting send: #onJVMShutdownStarting to: self;
		when: #JVMAbort send: #onJVMAbort to: self;
		when: #JVMExit: send: #onJVMExit: to: self;
		when: #JVMReset send: #onJVMReset to: self;
		yourself.

	#CUtodo.  "refine this"
	self jvmStatus: (aJVMStatusModel jvmIsLive ifTrue: ['JVM is running'] ifFalse: ['JVM is dead']).
!

onJVMAbort
	"private -- the JVM has aborted itself (and -- suprisingly -- hasn't taken Dolphin out with it!!)"

	self jvmStatus: 'JVM has aborted'.
!

onJVMExit: aNumber
	"private -- the JVM has exited itself (and -- suprisingly -- hasn't taken Dolphin out with it!!)"

	self jvmStatus: 'JVM has exited'.
!

onJVMIsBorn
	"private -- the JVM has started its intialisation sequence"

	self jvmStatus: 'JVM is initialising'.
!

onJVMIsDead
	"private -- the JVM is now dead"

	| timestamp date time str |

	timestamp := self model diedAt.
	date := timestamp first displayString.
	time := timestamp second displayString.
	str := ' JVM died at %s %s' sprintfWith: time with: date.

	self jvmStatus: str.
	self view refreshContents.
!

onJVMIsLive
	"private -- the JVM has finished its intialisation sequence and is now fully live"

	self jvmStatus: 'JVM is running'.
!

onJVMReset
	"private -- the JVM has reset itself"

	self refresh.!

onJVMShutdownStarting
	"private -- the JVM is about to start a shutdown process"

	self jvmStatus: 'JVM is shutting down'.
!

onViewClosed
	"our view has been closed -- cleanup"

	self model removeEventsTriggeredFor: self.
	super onViewClosed.!

polyViewerStatusText
	"answer the text to use in the status line when the receiver is acting as a page in a PolyViewerShell."

	^ jvmStatus.!

refresh
	"private -- refresh ourselves"
#subclassResponsibility.

	"default implementation"
	self model: self model.! !
!JVMStatusPageAbstract categoriesFor: #help!commands!public! !
!JVMStatusPageAbstract categoriesFor: #helpFileName!constants!private! !
!JVMStatusPageAbstract categoriesFor: #initialize!initializing!private! !
!JVMStatusPageAbstract categoriesFor: #jvmStatus:!accessing!public! !
!JVMStatusPageAbstract categoriesFor: #model:!event handling!initializing!models!private! !
!JVMStatusPageAbstract categoriesFor: #onJVMAbort!event handling!private! !
!JVMStatusPageAbstract categoriesFor: #onJVMExit:!event handling!private! !
!JVMStatusPageAbstract categoriesFor: #onJVMIsBorn!event handling!private! !
!JVMStatusPageAbstract categoriesFor: #onJVMIsDead!event handling!private! !
!JVMStatusPageAbstract categoriesFor: #onJVMIsLive!event handling!private! !
!JVMStatusPageAbstract categoriesFor: #onJVMReset!event handling!private! !
!JVMStatusPageAbstract categoriesFor: #onJVMShutdownStarting!event handling!private! !
!JVMStatusPageAbstract categoriesFor: #onViewClosed!event handling!public! !
!JVMStatusPageAbstract categoriesFor: #polyViewerStatusText!accessing!public! !
!JVMStatusPageAbstract categoriesFor: #refresh!commands!private! !

JVMClassesPage guid: (GUID fromString: '{F4026FF0-01DC-4728-80DD-E6AABB9ADD0A}')!
JVMClassesPage comment: 'Copyright © Chris Uppal, 2002 - 2005.
chris.uppal@metagnostic.org

A pluggin page for the Status Monitor.  Shows a hierarchy of known Java classes and interfaces.  If the Wrapper Wizard is installed then it can also launch that.'!
!JVMClassesPage categoriesForClass!Unclassified! !
!JVMClassesPage methodsFor!

browseInstanceClass
	"command -- launch a browser on the instance class of the selected class static"

	self selectedClass ifNotNil: [:it | it instanceClass ghostClassRoot browse].!

browseJavaDoc
	"command -- launch a JavaDoc browser on the selected class, if any"

	| classStatic path |

	classStatic := self selectedClass.
	path := (classStatic notNil and: [classStatic canBeBrowsed])
			ifTrue: [classStatic browserString]
			ifFalse: ['index.html'].

	self javadocRoots do:
		[:root | (self browseJavaDoc: path in: root) ifTrue: [^ self]].

	MessageBox errorMsg: ('Cannot browse to JavaDoc for ' , classStatic name) caption: 'Cannot find file'.!

browseJavaDoc: anFilename in: aFolderName
	"private -- attempt to open a browser on the given filename, answer true if we managed it"

	[ShellLibrary default shellOpen: anFilename directory: aFolderName]
		on: Win32Error
		do: [:err | ^ false].

	^ true.
!

browseStaticClass
	"command -- launch a browser on the class of the selected class static"

	self selectedClass ifNotNil: [:it | it class ghostClassRoot browse].!

buildClassMenu: aMenu
	"private -- fill in aMenu with commands appropriate to
	the currently selected object"

	aMenu clear.

	(self class classMenu)
		cleanUp;
		populateMenu: aMenu.

	^ aMenu.!

classesPresenter
	"private -- answer the presenter named 'Classes'"

	^ self presenterNamed: 'Classes'.
!

classWrappersPresenter
	"private -- answer the presenter named 'ClassWrappers'"

	^ self presenterNamed: 'ClassWrappers'.
!

createComponents
	"private - create presenters for the necessary components"

	super createComponents.

	self add: TreePresenter new name: 'Classes'.
	self add: ListPresenter new name: 'InstanceWrappers'.
	self add: ListPresenter new name: 'ClassWrappers'.
	self add: TextPresenter new name: 'Inheritance'.
!

createSchematicWiring
	"private - arrange triggering between our components"

	super createSchematicWiring.

	self classesPresenter
		when: #selectionChanged send: #onClassSelected to: self;
		when: #drag: send: #onDragClass: to: self.

	self instanceWrappersPresenter
		when: #drag: send: #onDragMethod: to: self.

	self classWrappersPresenter
		when: #drag: send: #onDragMethod: to: self.
!

findClass
	"command -- find/load a class by name"

	| classname pattern class classes |

	classname := Prompter
		prompt: 'Enter a Java class name (wildcards allowed): '
		caption: 'Find a Java Class'.

	classname isNil ifTrue: [^ self].

	classname := classname trimBlanks.
	classname isEmpty ifTrue: [^ self].

	pattern := classname.
	(classname beginsWith: '*') ifFalse: [pattern := '*' , pattern].
	(classname endsWith: '*') ifFalse: [pattern := pattern , '*'].

	classes := ((self model jvm classRegistry allClasses)
				collect: [:each | each name])
					select: [:each | pattern match: each].

	names isEmpty ifTrue:
		[MessageBox
			notify: ('No Java class names match ' , pattern)
			caption: 'Find a Java class'.
		^ self].

	(classes includes: classname) ifTrue: [^ self findClass: classname].
	(classes size = 1) ifTrue: [^ self findClass: classes first].

	classname := ChoicePrompter
				choices: classes asSortedCollection
				caption: 'Find a Java class'.

	classname isNil ifFalse: [self findClass: classname].
!

findClass: aString
	"command -- find/load a class by name"

	| class |

	class := [self model jvm findClass: aString]
			on: JavaException
			do: [:err | ^ MessageBox errorMsg: err printString caption: ('Cannot find ' , aString)].
	self selectedClass: class.!

findOrLoadClass
	"command -- find/load a class by name"

	| classname class |

	classname := Prompter
		prompt: 'Enter a fully-qualified Java class name (no wildcards): '
		caption: 'Find/Load a Java Class'.

	classname isNil ifTrue: [^ self].

	classname := classname trimBlanks.
	classname isEmpty ifTrue: [^ self].

	self findClass: classname.
!

generateClassSideWrapper
	"command -- launch a wrapper generator wizard on the currenly selected class, if any"

	| static |

	static := self selectedClass.
	static isNil ifTrue: [^ self].
	static canBeWrapped ifFalse: [^ self].

	Smalltalk at: #JavaClassWrapperInstaller ifPresent:
		[:installerClass || target settings installer |
		target := installerClass suggestedDestinationForClassSideOf: static.
		settings := self model wrapperGeneratorSettings deepCopy.
		installer := installerClass forClassSideOf: static destination: target settings: settings.
		Smalltalk at: #JavaWrapperWizard ifPresent:
			[:wizardClass |
			wizardClass showOn: installer]].
!

generateInstanceSideWrapper
	"command -- launch a wrapper generator wizard on the currenly selected class, if any"

	| static |

	static := self selectedClass.
	static isNil ifTrue: [^ self].
	static canBeWrapped ifFalse: [^ self].

	Smalltalk at: #JavaClassWrapperInstaller ifPresent:
		[:installerClass || target settings installer |
		target := installerClass suggestedDestinationForInstanceSideOf: static.
		settings := self model wrapperGeneratorSettings deepCopy.
		installer := installerClass forInstanceSideOf: static destination: target settings: settings.
		Smalltalk at: #JavaWrapperWizard ifPresent:
			[:wizardClass |
			wizardClass showOn: installer]].!

helpFileName
	"private -- answer the filename of our help text; this
	is relative to the documentation directory"

	^ 'status-monitor-classes.html'.!

inheritancePresenter
	"private -- answer the presenter named 'Inheritance'"

	^ self presenterNamed: 'Inheritance'.
!

inspectClass
	"command -- launch an inspector on the selected class static"

	self selectedClass ifNotNil: [:it | it inspect].!

inspectClassfile
	"command -- launch an inspector on the selected class static's dissasembly"

	| class classfile |

	class := self selectedClass ifNil: [^ self].
	classfile := Smalltalk at: #JVMClassfile ifPresent: [:it | it fromJavaLangClass: class classObject].
	classfile isNil
		ifTrue: [MessageBox notify: 'No bytecode source found for ' , class name caption: 'Failed to disassemble classfile']
		ifFalse: [classfile inspect].!

instanceWrappersPresenter
	"private -- answer the presenter named 'InstanceWrappers'"

	^ self presenterNamed: 'InstanceWrappers'.
!

javadocRoots
	"private -- answer a collection of the names of folders that may contain
	JavaDoc documentation"

	| local global |

	local := self model jvm settings statusMonitorSettings javadocPath copyReplacing: $/ withObject: $\.
	global := self class globalJavadocPath copyReplacing: $/ withObject: $\.

	^ (local subStrings: ';') , (global subStrings: ';').!

model: aJVMStatusModel
	"private -- set the model for this Presenter"

	super model: aJVMStatusModel.

	self classesPresenter model: aJVMStatusModel classesTreeModel.

	self onClassSelected.
!

onAboutToDisplayMenu: aMenu
	"this is invoked when aMenu is about to be popped-up;
	update it appropriately"

	aMenu name == #dynamicClassMenu ifTrue:
		[^ self buildClassMenu: aMenu].
!

onClassSelected
	"private -- called when the a new class static is selected in the tree"

	| classStatic iWrappers cWrappers inheritance |

	iWrappers := cWrappers := #(). 
	inheritance := ''.

	classStatic := self selectedClass.
	classStatic isNil ifFalse:
		[iWrappers := classStatic generatedInstanceSelectors.
		cWrappers := classStatic generatedSelectors.
		inheritance := classStatic inheritanceStringWithStatus].

	self instanceWrappersPresenter list: iWrappers.
	self classWrappersPresenter list: cWrappers.
	self inheritancePresenter value: inheritance.
!

onDragClass: aDragDropSession
	"private -- a drag/drop session has been started from our class tree"

	aDragDropSession dragObjects do:
		[:each || class |
		class := each format: #Object.
		 each
			format: #String data: class name; 
			format: #Symbol data: class name asSymbol].

	aDragDropSession defaultOperation: #copy
!

onDragMethod: aDragDropSession
	"private -- a drag/drop session has been started from one of our method lists"

	aDragDropSession dragObjects do:
		[:each || selector |
		selector := each format: #Object.
		each
			format: #String data: selector asString; 
			format: #Symbol data: selector].

	aDragDropSession defaultOperation: #copy

!

onJVMIsDead
	"private -- the JVM is now dead"

	super onJVMIsDead.

	"hacky way of refreshing the class list without disturbing our status line"
	self classesPresenter model: self model classesTreeModel.
	self onClassSelected.
!

onViewOpened
	"called by the system as our window is opened"

	| v f |

	super onViewOpened.

	"if we might have lazy ghosts, then arrange to show them faded in the class tree"
	(Smalltalk includesKey: #JVMLazyClassStatic) ifTrue:
		[self classesPresenter view customDrawBlock: [:ctx | self ownerDrawClass: ctx. ctx := nil]].
!

ownerDrawClass: anNMTVCUSTOMDRAW
	"private -- called when Windows wants us to fill in the given NMTVCUSTOMDRAW object
	for an item in the class tree"

	| class |

	class := anNMTVCUSTOMDRAW item.
	class isNil ifTrue: [^ self].

	(class isUnresolved) ifTrue:
		[anNMTVCUSTOMDRAW forecolor: (anNMTVCUSTOMDRAW forecolor fadedBy: 3)].!

purgeClass
	"command -- purge the currently selected class, if any.
	NB: this is a development (of JNIPort) method *ONLY*, do
	not rely on its working as you'd expect, nor on its continuing
	existence"

	| class |

	class := self selectedClass.
	class isNil ifTrue: [^ self].
	class canBePurged ifFalse: [^ self].

	(MessageBox confirm: ('Are you really, really, sure you want to purge ' , class name , '?')) ifFalse: [^ self].

	class purge.!

queryCommand: aCommandQuery
	"private -- set the enabledness of a command"

	| class cmd enabled |

	super queryCommand: aCommandQuery.

	enabled := aCommandQuery isEnabled.
	cmd := aCommandQuery command.

	class := self selectedClass.

	"these require a selection"
	(enabled and: [#(
		#browseJavaDoc
		#inspectClass
		#inspectClassfile
		#browseInstanceClass
		#browseStaticClass
		#generateInstanceSideWrapper
		#generateClassSideWrapper
		#purgeClass
		#reloadClass
	) identityIncludes: cmd]) ifTrue:
		[enabled := class notNil].

	"these require a live JVM"
	(enabled and: [#(
		#inspectClass
		#inspectClassfile
		#findClass
		#findOrLoadClass
		#generateInstanceSideWrapper
		#generateClassSideWrapper
		#purgeClass
		#reloadClass
	) identityIncludes: cmd]) ifTrue:
		[enabled := self model jvmIsRunning].

	"these require wrapper generators to be installed"
	(enabled and: [#(
		#generateInstanceSideWrapper
		#generateClassSideWrapper
	) identityIncludes: cmd]) ifTrue:
		[enabled := (Smalltalk includesKey: #JavaWrapperWizard) and: [Smalltalk includesKey: #JavaClassWrapperInstaller]].

	"special cases"
	(enabled and: [cmd = #browseJavaDoc]) ifTrue:
		[enabled := class canBeBrowsed].
	(enabled and: [cmd = #purgeClass]) ifTrue:
		[enabled := class canBePurged].
	(enabled and: [cmd = #reloadClass]) ifTrue:
		[enabled := class canBePurged].
	(enabled and: [cmd = #generateInstanceSideWrapper]) ifTrue:
		[enabled := class canBeWrapped].
	(enabled and: [cmd = #generateClassSideWrapper]) ifTrue:
		[enabled := class canBeWrapped].
	(enabled and: [cmd = #inspectClassfile]) ifTrue:
		[enabled := Smalltalk includesKey: #JVMClassfile].

	"dynamic menus are always enabled"
	(cmd = #dynamicClassMenu) ifTrue:
		[enabled := true].

	aCommandQuery isEnabled: enabled.
!

reloadClass
	"command -- purge and reload the currently selected class, if any.
	NB: this is a development (of JNIPort) method *ONLY*, do
	not rely on its working as you'd expect, nor on its continuing
	existence"

	| class name |

	class := self selectedClass.
	class isNil ifTrue: [^ self].
	class canBePurged ifFalse: [^ self].

	name := class name.
	(MessageBox confirm: ('Are you really, really, sure you want to reload ' , name , '?')) ifFalse: [^ self].

	class purge.
	self findClass: name.!

selectedClass
	"private -- answer the currently selected JavaStatic (exclding fakes) or nil if there isn't one"

	| selected |

	selected := self classesPresenter selectionOrNil.

	^ (selected notNil and: [selected isFake not])
		ifTrue: [selected]
		ifFalse: [nil].!

selectedClass: aJavaStatic
	"private -- set the currently selected java class static"

	^ self classesPresenter selectionOrNil: aJavaStatic.
!

selectedClassMethod
	"private -- answer the currently selected class-side method or nil if there isn't one"

	^ self classWrappersPresenter selectionOrNil.
!

selectedInstanceMethod
	"private -- answer the currently selected instance-side method or nil if there isn't one"

	^ self instanceWrappersPresenter selectionOrNil.
!

setJavadocPath
	"command -- set the root directory for JavaDoc lookups"

	| path |

	path := self class globalJavadocPath.

	path := Prompter
		on: path
		prompt: 'Enter a ;-separated list of folders to search'
		caption: 'JavaDoc Path'.

	path notNil ifTrue: [self class globalJavadocPath: path].

! !
!JVMClassesPage categoriesFor: #browseInstanceClass!commands!public! !
!JVMClassesPage categoriesFor: #browseJavaDoc!commands!public! !
!JVMClassesPage categoriesFor: #browseJavaDoc:in:!helpers!private! !
!JVMClassesPage categoriesFor: #browseStaticClass!commands!public! !
!JVMClassesPage categoriesFor: #buildClassMenu:!event handling!menus!private! !
!JVMClassesPage categoriesFor: #classesPresenter!private!subpresenters! !
!JVMClassesPage categoriesFor: #classWrappersPresenter!private!subpresenters! !
!JVMClassesPage categoriesFor: #createComponents!initializing!private!subpresenters! !
!JVMClassesPage categoriesFor: #createSchematicWiring!event handling!initializing!private! !
!JVMClassesPage categoriesFor: #findClass!commands!public! !
!JVMClassesPage categoriesFor: #findClass:!commands!public! !
!JVMClassesPage categoriesFor: #findOrLoadClass!commands!public! !
!JVMClassesPage categoriesFor: #generateClassSideWrapper!commands!menus!public! !
!JVMClassesPage categoriesFor: #generateInstanceSideWrapper!commands!menus!public! !
!JVMClassesPage categoriesFor: #helpFileName!constants!private! !
!JVMClassesPage categoriesFor: #inheritancePresenter!private!subpresenters! !
!JVMClassesPage categoriesFor: #inspectClass!commands!public! !
!JVMClassesPage categoriesFor: #inspectClassfile!commands!public! !
!JVMClassesPage categoriesFor: #instanceWrappersPresenter!private!subpresenters! !
!JVMClassesPage categoriesFor: #javadocRoots!helpers!private! !
!JVMClassesPage categoriesFor: #model:!initializing!models!private! !
!JVMClassesPage categoriesFor: #onAboutToDisplayMenu:!event handling!menus!private! !
!JVMClassesPage categoriesFor: #onClassSelected!event handling!initializing!private! !
!JVMClassesPage categoriesFor: #onDragClass:!event handling!private! !
!JVMClassesPage categoriesFor: #onDragMethod:!event handling!private! !
!JVMClassesPage categoriesFor: #onJVMIsDead!event handling!private! !
!JVMClassesPage categoriesFor: #onViewOpened!event handling!public! !
!JVMClassesPage categoriesFor: #ownerDrawClass:!drawing!private! !
!JVMClassesPage categoriesFor: #purgeClass!commands!public! !
!JVMClassesPage categoriesFor: #queryCommand:!commands!menus!private! !
!JVMClassesPage categoriesFor: #reloadClass!commands!public! !
!JVMClassesPage categoriesFor: #selectedClass!accessing!private! !
!JVMClassesPage categoriesFor: #selectedClass:!accessing!private! !
!JVMClassesPage categoriesFor: #selectedClassMethod!accessing!private! !
!JVMClassesPage categoriesFor: #selectedInstanceMethod!accessing!private! !
!JVMClassesPage categoriesFor: #setJavadocPath!commands!public! !

!JVMClassesPage class methodsFor!

addCommonOptionsToClassMenu: aPolyViewerCommandList
	"private -- add any commnds to the given list that are appropriate for use in any session"

	aPolyViewerCommandList
		add: ((PolyViewerCommandItem new)
			command: #findClass;
			text: '&Find Java class';
			accelerator: 'CTRL+F';
			yourself).

	aPolyViewerCommandList
		add: ((PolyViewerCommandItem new)
			command: #findOrLoadClass;
			text: '&Find/Load Java class';
			accelerator: 'CTRL+SHIFT+F';
			yourself).

	aPolyViewerCommandList
		add: ((PolyViewerCommandItem new)
			command: #browseJavaDoc;
			text: '&Browse JavaDoc';
			accelerator: 'F12';
			yourself).
!

addDevelopmentOptionsToClassMenu: aPolyViewerCommandList
	"private -- add any commands to the given list that are appropriate for use in a development session"

	aPolyViewerCommandList
		add: PolyViewerCommandSeparator new.

	aPolyViewerCommandList
		 add: ((PolyViewerCommandItem new)
			command: #browseInstanceClass;
			text: 'Browse i&nstance class';
			accelerator: 'CTRL+B';
			yourself).

	aPolyViewerCommandList
		 add: ((PolyViewerCommandItem new)
			command: #browseStaticClass;
			text: 'Browse &static class';
			accelerator: 'CTRL+SHIFT+B';
			yourself).

	aPolyViewerCommandList
		 add: PolyViewerCommandSeparator new.

	aPolyViewerCommandList
		 add: ((PolyViewerCommandItem new)
			command: #generateInstanceSideWrapper;
			text: 'Generate &instance-side wrappers...';
			yourself).

	aPolyViewerCommandList
		 add: ((PolyViewerCommandItem new)
			command: #generateClassSideWrapper;
			text: 'Generate &class-side wrappers...';
			yourself).

!

addPrivateOptionsToClassMenu: aPolyViewerCommandList
	"private -- add any commands to the given list that are appropriate for use by a JNIPort aurhor (i.e me ;-)"

	aPolyViewerCommandList
		add: PolyViewerCommandSeparator new.

	aPolyViewerCommandList
		 add: ((PolyViewerCommandItem new)
			command: #inspectClass;
			text: 'Ins&pect class static';
			accelerator: 'CTRL+I';
			yourself).

	aPolyViewerCommandList
		 add: ((PolyViewerCommandItem new)
			command: #inspectClassfile;
			text: 'Inspect &binary classfile';
			accelerator: 'CTRL+C';
			yourself).

	aPolyViewerCommandList
		 add: PolyViewerCommandSeparator new.

	aPolyViewerCommandList
		 add: ((PolyViewerCommandItem new)
			command: #purgeClass;
			text: '&Purge class...';
			yourself).!

classMenu
	"answer a PolyViewerCommandList of the operations which are applicable to any
	selected class"

	| list |

	list := PolyViewerCommandList new.

	self addCommonOptionsToClassMenu: list.
	SessionManager current isRuntime ifFalse: [self addDevelopmentOptionsToClassMenu: list].
	(JNILibrary enableDevelopmentOptions) ifTrue: [self addPrivateOptionsToClassMenu: list].

	^ list.!

globalJavadocPath
	"answer the String search path for JavaDoc documentation.
	This is a ';'-separated list of folder names that is appended to
	the JVM-specific javadoc path"

	^ globalJavadocPath ifNil: [''].!

globalJavadocPath: aString
	"set the String search path for JavaDoc documentation.
	This is a ';'-separated list of folder names that is appended to
	the JVM-specific javadoc path"

	#CUtodo.  "keep this in the registry ?"

	globalJavadocPath := aString.!

polyViewerMenuCommands
	"Invoked by the PolyViewer framework.  Should answer a PolyViewerCommandList
	which defines a menu bar which will be merged into the menu bar of our enclosing
	PolyViewerShell instance"

	^ (PolyViewerCommandList new)

		"view menu"
		add: ((PolyViewerCommandList new)
			text: '&View';

			add: ((PolyViewerCommandItem new)
				command: #refresh;
				text: '&Refresh';
				accelerator: 'F5';
				yourself);

			add: PolyViewerCommandSeparator new;

			yourself);

		"class menu"
		add: (self classMenu
			text: '&Class';
			yourself);

		"tools menu"
		add: ((PolyViewerCommandList new)
			text: '&Tools';
			add: ((PolyViewerCommandList new)
				text: '&Options';
				add: PolyViewerCommandSeparator new;
				add: ((PolyViewerCommandItem new)
					command: #setJavadocPath;
					text: 'Global &JavaDoc path...';
					yourself);
				yourself);
			yourself);


		yourself.
! !
!JVMClassesPage class categoriesFor: #addCommonOptionsToClassMenu:!constants!menus!private! !
!JVMClassesPage class categoriesFor: #addDevelopmentOptionsToClassMenu:!constants!menus!private! !
!JVMClassesPage class categoriesFor: #addPrivateOptionsToClassMenu:!constants!menus!private! !
!JVMClassesPage class categoriesFor: #classMenu!constants!menus!public! !
!JVMClassesPage class categoriesFor: #globalJavadocPath!accessing!public! !
!JVMClassesPage class categoriesFor: #globalJavadocPath:!accessing!public! !
!JVMClassesPage class categoriesFor: #polyViewerMenuCommands!constants!menus!public! !

JVMClassloadersPage guid: (GUID fromString: '{52430599-50B0-49DE-A88E-CD760C605F43}')!
JVMClassloadersPage comment: 'Copyright © Chris Uppal, 2005.
chris.uppal@metagnostic.org

A pluggin page for the Status Monitor.  Shows a hierarchy of the ''supplementary classloaders''.'!
!JVMClassloadersPage categoriesForClass!Unclassified! !
!JVMClassloadersPage methodsFor!

activateClassloader
	"command -- activate the currently selected classloader, if any"

	| classloader |

	classloader := self selectedClassloader.
	classloader isNil ifTrue: [^ self].

	classloader activate.!

buildClassloaderMenu: aMenu
	"private -- fill in aMenu with commands appropriate to
	the currently selected object"

	aMenu clear.

	(self class classloaderMenu)
		cleanUp;
		populateMenu: aMenu.

	^ aMenu.!

classloaderClassPresenter
	"private -- answer the presenter named 'ClassloaderClass'"

	^ self presenterNamed: 'ClassloaderClass'.
!

classloaderEnabledPresenter
	"private -- answer the presenter named 'ClassloaderEnabled'"

	^ self presenterNamed: 'ClassloaderEnabled'.
!

classloaderNamePresenter
	"private -- answer the presenter named 'ClassloaderName'"

	^ self presenterNamed: 'ClassloaderName'.
!

classloaderPathPresenter
	"private -- answer the presenter named 'ClassloaderPath'"

	^ self presenterNamed: 'ClassloaderPath'.
!

classloadersPresenter
	"private -- answer the presenter named 'Classloaders'"

	^ self presenterNamed: 'Classloaders'.
!

createComponents
	"private - create presenters for the necessary components"

	super createComponents.

	self
		add: TreePresenter new name: 'Classloaders';
		add: (TextPresenter new) name: 'ClassloaderClass';
		add: (BooleanPresenter new) name: 'ClassloaderEnabled';
		add: (TextPresenter new) name: 'ClassloaderName';
		add: (TextPresenter new) name: 'ClassloaderPath';
!

createSchematicWiring
	"private -- arrange triggering between our components"

	self classloadersPresenter
		when: #selectionChanged send: #onClassloaderSelected to: self;
		yourself.

	^ super createSchematicWiring.
!

helpFileName
	"private -- answer the filename of our help text; this
	is relative to the documentation directory"

	^ 'status-monitor-classloaders.html'.!

model: aJVMStatusModel
	"private -- set the model for this Presenter"

	super model: aJVMStatusModel.

	self classloadersPresenter model: aJVMStatusModel supplementaryClassloadersTreeModel.

	self onClassloaderSelected.
!

onAboutToDisplayMenu: aMenu
	"this is invoked when aMenu is about to be popped-up;
	update it appropriately"

	aMenu name == #dynamicClassloaderMenu ifTrue:
		[^ self buildClassloaderMenu: aMenu].
!

onClassloaderSelected
	"private -- called when the selection in the classloader tree has changed"

	| classloader |

	classloader := self selectedClassloader.

	classloader isNil
		ifTrue:
			[self classloaderNamePresenter value: nil.
			self classloaderPathPresenter value: nil.
			self classloaderClassPresenter value: nil.
			self classloaderEnabledPresenter model: false asValue.
			self classloaderEnabledPresenter view hide]
		ifFalse:
			[self classloaderNamePresenter value: classloader name.
			self classloaderPathPresenter value: classloader path.
			self classloaderClassPresenter value: classloader classloaderClass.
			self classloaderEnabledPresenter model. (classloader aspectValue: #isEnabled).
			self classloaderEnabledPresenter view show].!

onViewOpened
	"called by the system as our window is opened"

	| v f |

	super onViewOpened.

	v := self classloaderNamePresenter view.
	f := v actualFont copy.
	f beBold.
	v font: f.
!

purgeClassloader
	"command -- purge the currently selected classloader, if any"

	| classloader |

	classloader := self selectedClassloader.
	classloader isNil ifTrue: [^ self].

	classloader purge.!

queryCommand: aCommandQuery
	"private -- set the enabledness of a command"

	| classloader jvm cmd enabled checked |

	super queryCommand: aCommandQuery.

	enabled := aCommandQuery isEnabled.
	checked := aCommandQuery isChecked.
	cmd := aCommandQuery command.

	classloader := self selectedClassloader.
	jvm := self model jvm.

	cmd = #toggleClassloadersInUse ifTrue:
		[enabled := jvm isRunning.
		checked := enabled and: [jvm usingSupplementaryClassloaders]].

	cmd = #purgeClassloader ifTrue:
		[enabled := classloader notNil and: [classloader isEnabled and: [classloader isActive]]].

	cmd = #activateClassloader ifTrue:
		[enabled := classloader notNil and: [classloader isEnabled and: [classloader isActive not]]].

	cmd = #toggleClassloaderEnabled ifTrue:
		[enabled := classloader notNil.
		checked := enabled and: [classloader isEnabled]].

	(cmd = #dynamicClassloaderMenu) ifTrue:
		[enabled := true].

	aCommandQuery
		isEnabled: enabled;
		isChecked: checked.
!

selectedClassloader
	"private -- answer the currently selected SupplementaryClassloader or nil if there isn't one"

	^ self classloadersPresenter selectionOrNil.!

toggleClassloaderEnabled
	"command -- toggle whether the the currently selected classloader, if any, is active"

	| classloader |

	classloader := self selectedClassloader.
	classloader isNil ifTrue: [^ self].

	classloader isEnabled: (classloader isEnabled not).!

toggleClassloadersInUse
	"command -- toggle whether the JVM is making use of its supplementary classloaders"

	| jvm |

	jvm := self model jvm.
	jvm isRunning ifFalse: [^ self].

	jvm useSupplementaryClassloaders: (jvm usingSupplementaryClassloaders not).! !
!JVMClassloadersPage categoriesFor: #activateClassloader!commands!public! !
!JVMClassloadersPage categoriesFor: #buildClassloaderMenu:!event handling!menus!private! !
!JVMClassloadersPage categoriesFor: #classloaderClassPresenter!private!subpresenters! !
!JVMClassloadersPage categoriesFor: #classloaderEnabledPresenter!private!subpresenters! !
!JVMClassloadersPage categoriesFor: #classloaderNamePresenter!private!subpresenters! !
!JVMClassloadersPage categoriesFor: #classloaderPathPresenter!private!subpresenters! !
!JVMClassloadersPage categoriesFor: #classloadersPresenter!private!subpresenters! !
!JVMClassloadersPage categoriesFor: #createComponents!initializing!private!subpresenters! !
!JVMClassloadersPage categoriesFor: #createSchematicWiring!event handling!initializing!private!subpresenters! !
!JVMClassloadersPage categoriesFor: #helpFileName!constants!private! !
!JVMClassloadersPage categoriesFor: #model:!initializing!models!private! !
!JVMClassloadersPage categoriesFor: #onAboutToDisplayMenu:!event handling!menus!private! !
!JVMClassloadersPage categoriesFor: #onClassloaderSelected!event handling!private! !
!JVMClassloadersPage categoriesFor: #onViewOpened!event handling!public! !
!JVMClassloadersPage categoriesFor: #purgeClassloader!commands!public! !
!JVMClassloadersPage categoriesFor: #queryCommand:!commands!menus!private! !
!JVMClassloadersPage categoriesFor: #selectedClassloader!accessing!private! !
!JVMClassloadersPage categoriesFor: #toggleClassloaderEnabled!commands!public! !
!JVMClassloadersPage categoriesFor: #toggleClassloadersInUse!commands!public! !

!JVMClassloadersPage class methodsFor!

classloaderMenu
	"answer a PolyViewerCommandList of the operations which are applicable to any
	selected class"

	^ (PolyViewerCommandList new)

		 add: ((PolyViewerCommandItem new)
			command: #toggleClassloadersInUse;
			text: '&Use supplementary classloaders';
			yourself);

		 add: PolyViewerCommandSeparator new;

		 add: ((PolyViewerCommandItem new)
			command: #toggleClassloaderEnabled;
			text: '&Enabled';
			yourself);

		 add: ((PolyViewerCommandItem new)
			command: #activateClassloader;
			text: '&Activate';
			yourself);

		 add: ((PolyViewerCommandItem new)
			command: #purgeClassloader;
			text: '&Purge';
			yourself);

		 add: PolyViewerCommandSeparator new;

		 add: ((PolyViewerCommandItem new)
			command: #newClassloader;
			text: '&New';
			yourself);

		 add: ((PolyViewerCommandItem new)
			command: #deleteClassloader;
			text: '&Delete';
			yourself);

		yourself.!

polyViewerMenuCommands
	"Invoked by the PolyViewer framework.  Should answer a PolyViewerCommandList
	which defines a menu bar which will be merged into the menu bar of our enclosing
	PolyViewerShell instance"

	^ (PolyViewerCommandList new)

		"view menu"
		add: ((PolyViewerCommandList new)
			text: '&View';

			add: ((PolyViewerCommandItem new)
				command: #refresh;
				text: '&Refresh';
				accelerator: 'F5';
				yourself);

			add: PolyViewerCommandSeparator new;

			yourself);

		"classload menu"
		add: (self classloaderMenu
			text: '&Classloader';
			yourself);


		yourself.
! !
!JVMClassloadersPage class categoriesFor: #classloaderMenu!constants!menus!public! !
!JVMClassloadersPage class categoriesFor: #polyViewerMenuCommands!constants!menus!public! !

JVMDBPrintfPage guid: (GUID fromString: '{D2B545F1-B7A9-4325-B5D0-DDFD8F25564C}')!
JVMDBPrintfPage comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org

A pluggin page for the Status Monitor.  Shows debugging output from the JVM (this is the debig output from the *JVM* which is not the same thing as console output, or the new Java logging APIs which are all about output from the *program*).

Be carefull, hooking the debugging output is the easiest way to get deadlocks.  If you are seeing deadlocks try:
	- make sure you are using the JNIHelper stuff.
	- hook the output but configure "useVFPrintfRedirection" too, which will send the debug to the windows debug facility.
	- don''t use the hook.
If none of them fix it, then the problems is probably not caused by the hooks...
'!
!JVMDBPrintfPage categoriesForClass!Unclassified! !
!JVMDBPrintfPage methodsFor!

addToTrace: aString
	"private -- add aString to the end of our trace panel"

	(self tracePresenter view)
		caretPosition: 0;
		replaceSelection: aString.
!

addToTrace: aString color: aColor
	"private -- add aString to the end of our trace panel"

	(self tracePresenter view)
		caretPosition: 0;
		selectionColor: aColor;
		replaceSelection: aString.!

checkHooks
	"private -- check to see if our JVM actually hooks the Java runtime's debug stream,
	and print a message if not"

	| jvm settings msg |

	jvm := self model jvm ifNil: [^ true].
	settings := jvm settings jniPortSettings.	"this is undoubtedly a rule-of-Demeter violation!!"

	(settings useVFPrintfHook and: [settings useVFPrintfRedirection not])
		ifTrue: [^ true].

	msg := settings useVFPrintfHook
			ifTrue: ['This JVM instance diverts the Java runtime''s debugging output
to the Window''s debug stream.']
			ifFalse: ['This JVM instance does not collect the Java runtime''s debugging output.'].

	self
		clearTrace;
		addToTrace: msg color: self warnColor;
		addToTrace: String lineDelimiter.

	^ false.!

clearTrace
	"command -- clear any accumulated trace"

	self tracePresenter view clear.
!

createComponents
	"private - create presenters for the necessary components"

	super createComponents.

	self add: (TextPresenter on: nil) name: 'Trace'.
!

helpFileName
	"private -- answer the filename of our help text; this
	is relative to the documentation directory"

	^ 'status-monitor-debug.html'.!

model: aJVMStatusModel
	"private -- set the model for this Presenter"

	super model: aJVMStatusModel.

	self clearTrace.
	self checkHooks.
	self model
		when: #JVMMessage:address: send: #onJVMMessage:address: to: self.
!

onJVMMessage: aString address: anExternalAddress
	"private -- the JVM has attempted to vprintf() aString to anExternalAddress"

	"for now, we ignore the external address.
	Note that we don't timestamp the string since JVMs have a habbit of sending messages
	as a bunch of little printf()s"
	self addToTrace: aString.!

onViewOpened
	"called by the system as our window is opened"

	super onViewOpened.
	self checkHooks.!

refresh
	"private -- refresh ourselves"

	"ignore it or else we'll clear our trace"!

tracePresenter
	"private -- answer the presenter named 'trace'"

	^ self presenterNamed: 'Trace'.
!

warnColor
	"private -- answer the colour to use for mesages written to Java stderr"

	^ Color yellow.! !
!JVMDBPrintfPage categoriesFor: #addToTrace:!helpers!private! !
!JVMDBPrintfPage categoriesFor: #addToTrace:color:!helpers!private! !
!JVMDBPrintfPage categoriesFor: #checkHooks!helpers!private! !
!JVMDBPrintfPage categoriesFor: #clearTrace!commands!public! !
!JVMDBPrintfPage categoriesFor: #createComponents!initializing!private!subpresenters! !
!JVMDBPrintfPage categoriesFor: #helpFileName!constants!private! !
!JVMDBPrintfPage categoriesFor: #model:!event handling!initializing!models!private! !
!JVMDBPrintfPage categoriesFor: #onJVMMessage:address:!event handling!private! !
!JVMDBPrintfPage categoriesFor: #onViewOpened!event handling!public! !
!JVMDBPrintfPage categoriesFor: #refresh!commands!private! !
!JVMDBPrintfPage categoriesFor: #tracePresenter!private!subpresenters! !
!JVMDBPrintfPage categoriesFor: #warnColor!constants!private! !

JVMStatusPage guid: (GUID fromString: '{C2C30993-CD0D-427C-B2CB-C564B2129126}')!
JVMStatusPage comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org

A pluggin page for the Status Monitor.  Shows an updating list of some indexes of the JVM''s health.'!
!JVMStatusPage categoriesForClass!Unclassified! !
!JVMStatusPage methodsFor!

canShutdownJVM
	"private -- answer true iff we may perform the #shutdownJVM command ?"

	^ self model canShutdownJVM.
!

createComponents
	"private - create presenters for the necessary components"

	super createComponents.

	self add: ListPresenter new name: 'StatusList'.
!

helpFileName
	"private -- answer the filename of our help text; this
	is relative to the documentation directory"

	^ 'status-monitor-status.html'.!

model: aJVMStatusModel
	"private -- set the model for this Presenter"

	super model: aJVMStatusModel.
	self statusListPresenter model: aJVMStatusModel statusList.
!

queryCommand: aCommandQuery
	"set the enablement, etc, of any commands"

	| cmd enabled |

	super queryCommand: aCommandQuery.

	cmd := aCommandQuery command.
	enabled := aCommandQuery isEnabled.

	cmd == #shutdownJVM ifTrue: [enabled := self canShutdownJVM].

	aCommandQuery isEnabled: enabled.!

shouldShutdownJVM
	"private -- ensure that the user wants to shutdown the JVM, and is aware of the implications
	of doing so"

	| answer |

	answer := (MessageBox new)
			caption: 'Shutdown JVM ?';
			text: self warnAboutShutdown;
			warning;
			yesNo;
			open.

	^ answer = #yes.!

shutdownJVM
	"command -- shutdown the selected JVM"

	(self canShutdownJVM and: [self shouldShutdownJVM]) ifTrue:
		[self model shutdownJVM].!

statusListPresenter
	"private -- answer the presenter named 'StatusList'"

	^ self presenterNamed: 'StatusList'.
!

warnAboutShutdown
	"private -- answer a String asking if the user really wants to bugger up their JVM session"

	SessionManager current isRuntime ifTrue: [^ 'Are you sure you want to shutdowm the Java runtime ?'].

	^
'Warning:  it is possible that, after you have closed the JVM, any attempt to
create a new one will instantly crash Dolphin.  To avoid this situation you must
close Dolphin and restart before you next create another JVM.

Are you sure you want to continue ?'.! !
!JVMStatusPage categoriesFor: #canShutdownJVM!commands!private! !
!JVMStatusPage categoriesFor: #createComponents!initializing!private!subpresenters! !
!JVMStatusPage categoriesFor: #helpFileName!constants!private! !
!JVMStatusPage categoriesFor: #model:!initializing!models!private! !
!JVMStatusPage categoriesFor: #queryCommand:!commands!public! !
!JVMStatusPage categoriesFor: #shouldShutdownJVM!private!testing! !
!JVMStatusPage categoriesFor: #shutdownJVM!commands!public! !
!JVMStatusPage categoriesFor: #statusListPresenter!private!subpresenters! !
!JVMStatusPage categoriesFor: #warnAboutShutdown!constants!private! !

JVMStatusShell guid: (GUID fromString: '{1A2A6B94-D27B-49E2-9D39-046E90BE42CE}')!
JVMStatusShell comment: 'Copyright © Chris Uppal, 2002 - 2005.
chris.uppal@metagnostic.org

Control panel for the JVM.

Holds a few pages which allow you to monitor/control some aspects of the JVM''s operation.'!
!JVMStatusShell categoriesForClass!Unclassified! !
!JVMStatusShell methodsFor!

bugs
	"command -- display the bugs-box"

	MessageBox
		warning: self class bugs
		caption: ('Known bugs in JNIPort').!

buildAlphaBlendMenu: aMenu
	"private -- fill in aMenu with commands various possible levels of alpha blend"

	aMenu clear.

	"these numbers are arbitrary"
	#(
		('None'	nil)
		('4/4'		255)
		('3/4'		224)
		('1/2'		192)
		('1/4'		128)
	) do:
		[:each || command item |
		command := Message selector: #setAlphaBlend: argument: each second.
		item := CommandMenuItem command: command description: each first.
		item isRadioButtonStyle: true.
		aMenu addItem: item].

	^ aMenu.!

buildSampleSizeMenu: aMenu
	"private -- fill in aMenu with commands various possible sample sizes"

	| text command item |

	aMenu clear.

	#(32 64 128 256 512 1024 2048) do:
		[:i |
		text := '%d samples' sprintfWith: i.
		command := Message selector: #sampleSize: argument: i.
		item := CommandMenuItem command: command description: text.
		item isRadioButtonStyle: true.
		aMenu addItem: item].

	^ aMenu.!

buildSelectJVMMenu: aMenu
	"private -- fill in aMenu with commands to select a running JVM"

	| text command item |

	aMenu clear.

	JVM runningInstances do:
		[:each |
		text := each name copyReplaceAll: '&' with: '&&'.
		command := Message selector: #selectJVM: argument: each.
		item := CommandMenuItem command: command description: text.
		item isRadioButtonStyle: true.
		aMenu addItem: item].

	^ aMenu.!

buildStartJVMMenu: aMenu
	"private -- fill in aMenu with commands to start predefined JVMs"

	| text command item |

	aMenu clear.

	JVMSettings predefined do:
		[:each | aMenu
				addCommand: (Message selector: #startJVM: argument: each)
				description: (each name copyReplaceAll: '&' with: '&&')].

	^ aMenu.!

buildUpdateIntervalMenu: aMenu
	"private -- fill in aMenu with commands various possible update intervals"

	| seconds text command item |

	aMenu clear.

	command := Message selector: #updateInterval: argument: 0.
	item := CommandMenuItem command: command description: '&Off'.
	item isRadioButtonStyle: true.
	aMenu addItem: item.

	aMenu addItem: DividerMenuItem new.

	#(1 2 3 5 10 15 30) do:
		[:i |
		text := '%d second%s' sprintfWith: i with: (i = 1 ifTrue: [''] ifFalse: ['s']).
		seconds := i.
		command := Message selector: #updateInterval: argument: seconds.
		item := CommandMenuItem command: command description: text.
		item isRadioButtonStyle: true.
		aMenu addItem: item].

	aMenu addItem: DividerMenuItem new.

	#(1 2 3 5 10 15 30) do:
		[:i |
		text := '%d minute%s' sprintfWith: i with: (i = 1 ifTrue: [''] ifFalse: ['s']).
		seconds := i * 60.
		command := Message selector: #updateInterval: argument: seconds.
		item := CommandMenuItem command: command description: text.
		item isRadioButtonStyle: true.
		aMenu addItem: item].

	aMenu addItem: DividerMenuItem new.

	#(1 2 3 12) do:
		[:i |
		text := '%d hour%s' sprintfWith: i with: (i = 1 ifTrue: [''] ifFalse: ['s']).
		seconds := i * 60 * 60.
		command := Message selector: #updateInterval: argument: seconds.
		item := CommandMenuItem command: command description: text.
		item isRadioButtonStyle: true.
		aMenu addItem: item].

	^ aMenu.!

canEditPredefinedJVMSettings
	"private -- answer whether we can edit the JVM settings"

	^ Smalltalk includesKey: #PublishedAspectInspector.!

createSchematicWiring
	"private - arrange triggering between our components"

	super createSchematicWiring.

	"this may as well go here as anywhere"
	JVM
		when: #JVMAdded: send: #onJVMAdded: to: self;
		when: #JVMRemoved: send: #onJVMRemoved: to: self.!

editPredefinedJVMSettings
	"command -- open a PAI on the predefined JVM settings"

	| shell |

	self canEditPredefinedJVMSettings ifFalse: [^ self].

	shell := (Smalltalk at: #PublishedAspectInspector) shellOn: JVMSettings.

	shell topShell caption: 'Predefined JVM settings'.
!

help
	"command -- display the help for the Status Monitor.
	NB: this will not often be called, since the focus will normally
	be in one of our plugin pages, and they have their own
	help"

	| locator file |

	"this will have to be recompiled if the package we live in changes its name"
	locator := PackageRelativeFileLocator packageNamed: ##(self owningPackage name).
	file := locator localFileSpecFor: 'Docs\JNIPort\status-monitor.html'.
	[ShellLibrary default shellOpen: file]
		on: Error
		do: [:err | MessageBox
			notify: ('Sorry.  No help for ' , self class toolName , ' is available')
			caption: 'JVM Status Help'].
!

inspectJVM
	"command -- inspect the currently selected JVM.
	NB: this is a development (of JNIPort) method *ONLY*, do
	not rely on its continuing existence"

	self model jvm inspect.
!

loadPredefinedJVMSettings
	"command -- load the predefined JVM settings from the default place in the registry"

	JVMSettings loadFromRegistry.!

model: aJVMStatusModel
	"switch our model to that given"

	self model notNil ifTrue: [self model detatch].

	super model: aJVMStatusModel.

	self model attatch.!

onAboutToDisplayMenu: aMenu
	"private -- this is invoked when aMenu is about to be popped-up;
	update it appropriately"

	aMenu name == #dynamicUpdateIntervalMenu ifTrue:
		[^ self buildUpdateIntervalMenu: aMenu].

	aMenu name == #dynamicSampleSizeMenu ifTrue:
		[^ self buildSampleSizeMenu: aMenu].

	aMenu name == #dynamicStartJVMMenu ifTrue:
		[^ self buildStartJVMMenu: aMenu].

	aMenu name == #dynamicSelectJVMMenu ifTrue:
		[^ self buildSelectJVMMenu: aMenu].

	aMenu name == #dynamicAlphaBlendMenu ifTrue:
		[^ self buildAlphaBlendMenu: aMenu].

	^ super onAboutToDisplayMenu: aMenu.
!

onJVMAdded: aJVM
	"a new JVM has started running"

	"if we are currently displaying a dead JVM then switch to the new one"
	self model jvmIsLive ifFalse: [self selectJVM: aJVM].!

onJVMRemoved: aJVM
	"an existing JVM has died"

	"NB: we flash even if we are not currently showing the newly dead JVM"
	self view isOpen ifTrue: [self view flash: 3].
!

onViewClosed
	"our view has been closed -- cleanup"

	self model
		removeEventsTriggeredFor: self;
		detatch.
	super onViewClosed.!

queryCommand: aCommandQuery
	"private -- set the enabledness of a command"

	| cmd enabled checked |

	super queryCommand: aCommandQuery.
	cmd := aCommandQuery command.
	cmd isNil ifTrue: [^ self].

	enabled := aCommandQuery isEnabled.
	checked := aCommandQuery isChecked.

	(#(
		#dynamicUpdateIntervalMenu #dynamicSampleSizeMenu #resetJVMRegistries #inspectJMV
	) includes: cmd) ifTrue:
		[enabled := self model jvmIsLive].

	cmd == #dynamicSelectJVMMenu ifTrue:
		[enabled := JVM runningInstances notEmpty].

	cmd == #dynamicStartJVMMenu ifTrue:
		[enabled := JVMSettings predefined notEmpty].

	cmd == #dynamicAlphaBlendMenu ifTrue:
		[enabled := true].

	cmd asSymbol == #selectJVM: ifTrue:
		[checked := cmd arguments first = self model jvm].

	cmd asSymbol == #updateInterval: ifTrue:
		[checked := cmd arguments first = self model updateInterval].

	cmd asSymbol == #sampleSize: ifTrue:
		[checked := cmd arguments first = self model sampleSize].

	cmd asSymbol == #editPredefinedJVMSettings ifTrue:
		[enabled := self canEditPredefinedJVMSettings].

	cmd asSymbol == #setAlphaBlend: ifTrue:
		[enabled := true.
		checked := cmd arguments first = alphaBlend].

	aCommandQuery isEnabled: enabled.
	aCommandQuery isChecked: checked.!

resetJVMRegistries
	"command -- get the selected JVM to reset all its registries.
	NB: this is a development (of JNIPort) method *ONLY*, do not rely
	on its continuing existence, nor expect that it will work at all"

	| class |

	(MessageBox confirm: ('Are you really, *really*, REALLY, sure you want to purge the registries ?')) ifFalse: [^ self].

	self model jvm resetRegistries.
!

sampleSize: anInteger
	"command -- set the number of samples kept by our status model"

	self model sampleSize: anInteger.!

savePredefinedJVMSettings
	"command -- save the predefined JVM settings to the default place in the registry"

	JVMSettings saveToRegistry.!

selectJVM: aJVM
	"command -- select the running JVM"

	self model: (JVMStatusModel for: aJVM).!

setAlphaBlend: anIntegerOrNil
	"set the alpha blend of our shell to that given by anInteger (in the range (1 to: 255)"

	anIntegerOrNil = alphaBlend ifTrue: [^ self].

	(anIntegerOrNil isNil = alphaBlend isNil) ifFalse: [self view isLayered: anIntegerOrNil notNil].

	alphaBlend := anIntegerOrNil.
	alphaBlend notNil ifTrue: [self view alphaBlend: alphaBlend].
	!

shouldStartJVM
	"private -- if we have already started a JVM in this session, then warn before starting another one. 
	Answer true iff it's OK to go ahead"

	| answer |

	JVM instancesStarted < 1 ifTrue: [^ true].

	answer := (MessageBox new)
			caption: 'Start another JVM ?';
			text: self warnAboutStartingJVM;
			warning;
			yesNo;
			open.

	^ answer = #yes.!

startJVM: aJVMSettings
	"command -- start the given JVM"

	"start it in the background since it may take some time"
	self shouldStartJVM ifTrue: [[JVM newWithSettings: aJVMSettings] fork].!

todo
	"command -- display the todo-box"

	MessageBox
		notify: self class todo
		caption: ('Known deficiencies in JNIPort').!

updateCaption
	"private - update the shell caption after some change"

	self captionExtension: self model displayString.!

updateInterval: aNumberOfSeconds
	"command -- set the interval between updates of our status model"

	self model updateInterval: aNumberOfSeconds.!

warnAboutStartingJVM
	"private -- answer a String asking if the user really wants to crash their image"

	SessionManager current isRuntime ifTrue: [^ 'Are you sure you want to start a new Java runtime ?'].

	^
'Please note: the Sun J2SDK (at least up to version 1.4.0) is unable to start more than
one JVM in the same program run.  Since the guys at Sun are, shall we say, sometimes
a little careless, the Java DLL doesn''t always report an error, but can simply crash the calling
program -- in this case taking out the Dolphin IDE.

Are you sure that you want to continue ?'.! !
!JVMStatusShell categoriesFor: #bugs!commands!public! !
!JVMStatusShell categoriesFor: #buildAlphaBlendMenu:!menus!private! !
!JVMStatusShell categoriesFor: #buildSampleSizeMenu:!menus!private! !
!JVMStatusShell categoriesFor: #buildSelectJVMMenu:!menus!private! !
!JVMStatusShell categoriesFor: #buildStartJVMMenu:!menus!public! !
!JVMStatusShell categoriesFor: #buildUpdateIntervalMenu:!menus!private! !
!JVMStatusShell categoriesFor: #canEditPredefinedJVMSettings!commands!private! !
!JVMStatusShell categoriesFor: #createSchematicWiring!event handling!initializing!private! !
!JVMStatusShell categoriesFor: #editPredefinedJVMSettings!commands!public! !
!JVMStatusShell categoriesFor: #help!commands!public! !
!JVMStatusShell categoriesFor: #inspectJVM!commands!public! !
!JVMStatusShell categoriesFor: #loadPredefinedJVMSettings!commands!public! !
!JVMStatusShell categoriesFor: #model:!initializing!models!public! !
!JVMStatusShell categoriesFor: #onAboutToDisplayMenu:!event handling!menus!private! !
!JVMStatusShell categoriesFor: #onJVMAdded:!event handling!private! !
!JVMStatusShell categoriesFor: #onJVMRemoved:!event handling!private! !
!JVMStatusShell categoriesFor: #onViewClosed!event handling!public! !
!JVMStatusShell categoriesFor: #queryCommand:!commands!menus!public! !
!JVMStatusShell categoriesFor: #resetJVMRegistries!commands!public! !
!JVMStatusShell categoriesFor: #sampleSize:!commands!public! !
!JVMStatusShell categoriesFor: #savePredefinedJVMSettings!commands!public! !
!JVMStatusShell categoriesFor: #selectJVM:!commands!public! !
!JVMStatusShell categoriesFor: #setAlphaBlend:!commands!public! !
!JVMStatusShell categoriesFor: #shouldStartJVM!checking!instances!private! !
!JVMStatusShell categoriesFor: #startJVM:!commands!public! !
!JVMStatusShell categoriesFor: #todo!commands!public! !
!JVMStatusShell categoriesFor: #updateCaption!private!updating! !
!JVMStatusShell categoriesFor: #updateInterval:!commands!public! !
!JVMStatusShell categoriesFor: #warnAboutStartingJVM!constants!private! !

!JVMStatusShell class methodsFor!

about
	"answer a string very briefly describing ourself"

	^ 'JVM Status Monitor.  Version 1.
Copyright © Chris Uppal 2002 - 2005.
chris.uppal@metagnostic.org'.
!

applicationMenu
	"Invoked by the PolyViewer framework.  Should answer a PolyViewerCommandList
	which defines a menu bar which will be merged into the menu bar of our enclosing
	PolyViewerShell instance"

	| list jvmList viewList |

	list := (PolyViewerCommandList new)

		"view menu"
		add: ((viewList := PolyViewerCommandList new)
			text: '&View';

			add: ((PolyViewerCommandList new)
				text: '&Update interval';
				name: #dynamicUpdateIntervalMenu;
				yourself);

			add: PolyViewerCommandSeparator new;

			yourself);

		"JVMs menu"
		add: ((jvmList := PolyViewerCommandList new)
			text: '&JVM';

			add: ((PolyViewerCommandList new)
				text: 'Select &JVM';
				name: #dynamicSelectJVMMenu;
				yourself);

			add: PolyViewerCommandSeparator new;

			add: ((PolyViewerCommandList new)
				text: '&Start JVM';
				name: #dynamicStartJVMMenu;
				yourself);

			add: PolyViewerCommandSeparator new;

			add: ((PolyViewerCommandList new)
				text: '&Predefined settings';

				add: ((PolyViewerCommandItem new)
					text: '&Edit...';
					command: #editPredefinedJVMSettings;
					yourself);
	
				add: ((PolyViewerCommandItem new)
					text: '&Save to registry';
					command: #savePredefinedJVMSettings;
					yourself);
	
				add: ((PolyViewerCommandItem new)
					text: '&Load from registry';
					command: #loadPredefinedJVMSettings;
					yourself);

				yourself);
	
			yourself);

		yourself.

	"if Udo Schneider's US Layered View stuff is installed, then we can add alpha-blending ;-)"
	(#(#isLayered: #alphaBlend:) allSatisfy: [:each | View canUnderstand: each]) ifTrue:
		[viewList
			add: ((PolyViewerCommandList new)
				text: 'Alpha &blend';
				name: #dynamicAlphaBlendMenu;
				yourself);
			add: PolyViewerCommandSeparator new].

	JNILibrary enableDevelopmentOptions ifTrue:
		[jvmList
			add: PolyViewerCommandSeparator new;
			add: ((PolyViewerCommandItem new)
				text: '&Inspect...';
				command: #inspectJVM;
				yourself);;
			add: ((PolyViewerCommandItem new)
				text: '&Reset class registries...';
				command: #resetJVMRegistries;
				yourself)].

	^ list.!

bugs
	"answer a String describing the less than outstanding work"

	^ JVM bugs.!

defaultModel
	"answer the model to use if no other is given"

	^ JVMStatusModel default.!

help
	"answer our help text"

	^ 'Sorry, no help for the JVM Status monitor is yet available'.!

icon
	"answer an Icon representing the receiver"

	^ JVM icon.!

initialize
	"private -- class-side initialisation.

		self initialize.
	"

	self reuseIfOpen: self superclass reuseIfOpen.
	self registerAsTool.
!

standardPages
	"answer the list of PolyViewerPages to use for displaying JVMStatusPresenters"

	| pages |

	pages :=  super standardPages.

	pages addLast: ((PolyViewerPageDescription new)
				presenterClass: JVMStatusPage;
				label: 'Status').

	pages addLast: ((PolyViewerPageDescription new)
				presenterClass: JVMClassesPage;
				label: 'Classes').

	pages addLast: ((PolyViewerPageDescription new)
				initiallyVisible: false;
				presenterClass: JVMClassloadersPage;
				label: 'Classloaders').

	pages addLast: ((PolyViewerPageDescription new)
				initiallyVisible: self useJVMPrintfPage;
				presenterClass: JVMDBPrintfPage;
				label: 'JVM Debug').

	^ pages.!

todo
	"answer a String describing the outstanding work"

	^ JVM todo.!

toolName
	"answer a name to use for this tool"

	^ 'JVM Status'.!

uninitialize
	"private -- class-side deinitialisation"

	self unRegisterAsTool.!

useJVMPrintfPage
	"private -- answer whether instances should show the JVM Debug page
	initially"

	"see if it's likely to be useful"
	((JVM runningInstances collect: [:each | each settings jniPortSettings]) anySatisfy:
		[:each | each useVFPrintfHook and: [each useVFPrintfRedirection not]])
			ifTrue: [^ true].
	((JVMSettings predefined collect: [:each | each jniPortSettings]) anySatisfy:
		[:each | each useVFPrintfHook and: [each useVFPrintfRedirection not]])
			ifTrue: [^ true].

	^ false.
! !
!JVMStatusShell class categoriesFor: #about!documentation!public! !
!JVMStatusShell class categoriesFor: #applicationMenu!constants!public! !
!JVMStatusShell class categoriesFor: #bugs!documentation!public! !
!JVMStatusShell class categoriesFor: #defaultModel!constants!models!public! !
!JVMStatusShell class categoriesFor: #help!documentation!public! !
!JVMStatusShell class categoriesFor: #icon!constants!public! !
!JVMStatusShell class categoriesFor: #initialize!development!initializing!private! !
!JVMStatusShell class categoriesFor: #standardPages!constants!public! !
!JVMStatusShell class categoriesFor: #todo!documentation!public! !
!JVMStatusShell class categoriesFor: #toolName!constants!displaying!public! !
!JVMStatusShell class categoriesFor: #uninitialize!development!initializing!private! !
!JVMStatusShell class categoriesFor: #useJVMPrintfPage!private!testing! !

"Binary Globals"!

"Resources"!

(ResourceIdentifier class: JVMClassesPage name: 'Default view') assign: (Object fromBinaryStoreBytes:
(ByteArray fromBase64String: 'IVNUQiAxIEYCDAABAAAAVmlld1Jlc291cmNlAAAAAA4BJABTVEJSZXNvdXJjZVNUQkJ5dGVBcnJh
eUFjY2Vzc29yUHJveHkAAAAAcgAAAKoSAAAhU1RCIDEgTggMAAoAAABTVEJWaWV3UHJveHkAAAAA
mgAAAAAAAABSAAAAEAAAAERvbHBoaW4gTVZQIEJhc2VSAAAADQAAAENvbnRhaW5lclZpZXdiAAAA
DwAAAAAAAAAAAAAAYgAAAAIAAACCAAAABAAAAAAAAEQBAAIAoAEAAAAAAAAGAQsAU3lzdGVtQ29s
b3IAAAAAHwAAAAAAAAAHAAAAAAAAAAAAAAAAAAAAoAEAAAYCEgBQcm9wb3J0aW9uYWxMYXlvdXQA
AAAA6gAAAAAAAADwAAAAYgAAAAAAAAAgAAAA6gAAAAAAAAAAAQAAYgAAAAIAAACaAQAAAAAAAJoA
AAAAAAAAUgAAABcAAABEb2xwaGluIENvbW1vbiBDb250cm9sc1IAAAAIAAAAVHJlZVZpZXdiAAAA
GwAAAAAAAACgAQAAYgAAAAIAAACCAAAABAAAACcAAUQBBAAAkAIAAEYECQACAAAAVHJlZU1vZGVs
AAAAAAAAAAAGAwgAVHJlZU5vZGUAAAAAAAAAAAAAAAAAAAAA6gAAAAAAAAAAAQAAYAIAAA4CEQBT
VEJTaW5nbGV0b25Qcm94eQAAAACaAAAAAAAAAFIAAAAHAAAARG9scGhpblIAAAAMAAAAU2VhcmNo
UG9saWN5ugAAAAAAAABSAAAACAAAAGlkZW50aXR5AAAAAAAAAAAPAAAARgUEAAIAAABNZW51AAAA
AAAAAAAQAAAAYgAAAAEAAABGAQ8AAQAAAERpdmlkZXJNZW51SXRlbQAAAAABEAAAUgAAAAAAAAC6
AAAAAAAAAFIAAAAQAAAAZHluYW1pY0NsYXNzTWVudQAAAAAAAAAAkAIAAAAAAACCAAAACAAAAOsE
//8AAAAAmgAAAAAAAADAAQAAUgAAABEAAABCYXNpY0xpc3RBYnN0cmFjdJoAAAAAAAAAsAIAAFIA
AAASAAAASWNvbmljTGlzdEFic3RyYWN0WgMAAAAAAACaAAAAAAAAAMABAABSAAAAEAAAAEljb25J
bWFnZU1hbmFnZXK6AAAAAAAAAFIAAAAHAAAAY3VycmVudAAAAAAGAgcATWVzc2FnZQAAAAC6AAAA
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
/wEAAACgEAAAYgYAAAAAAAByAAAALAAAACwAAAAAAAAAAQAAAP////////////////////8AAAAA
AAAAAKkAAADwAAAAygAAAAAAAADQAAAAYAIAAKAGAAAAAAAAFQAAAGIFAAAAAAAAygAAAAAAAADQ
AAAAYgAAAAEAAACiBQAAAAAAAMAFAABiAAAAAgAAAPIFAAAAAAAAYQEAAAsAAADyBQAAAAAAAFMB
AADhAQAAsAgAAGIGAAAAAAAAcgAAACwAAAAsAAAAAAAAAAEAAAD/////////////////////sAAA
AAUAAABZAQAA9QAAAMoAAAAAAAAA0AAAAGIAAAAEAAAAgAkAAPALAACQDQAAoBAAAKAGAAAAAAAA
EwAAAKAGAAAAAAAAEwAAAEYFBAADAAAASWNvbgAAAAAAAAAAEAAAAA4CEQBTVEJTaW5nbGV0b25Q
cm94eQAAAACaAAAAAAAAAFIAAAAHAAAARG9scGhpblIAAAAYAAAASW1hZ2VSZWxhdGl2ZUZpbGVM
b2NhdG9yugAAAAAAAABSAAAABwAAAGN1cnJlbnRSAAAAEQAAAENvbnRhaW5lclZpZXcuaWNvDgIf
AFNUQkV4dGVybmFsUmVzb3VyY2VMaWJyYXJ5UHJveHkAAAAAUgAAABAAAABkb2xwaGluZHIwMDUu
ZGxsAAAAAA=='))!

(ResourceIdentifier class: JVMClassloadersPage name: 'Default view') assign: (Object fromBinaryStoreBytes:
(ByteArray fromBase64String: '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
AAAsAAAAAAAAAAEAAAD/////////////////////AAAAAAAAAACGAQAAFQAAAMoAAAAAAAAA0AAA
ADADAABABgAAAAAAABMAAAAGAw4ARm9ybUxheW91dFNwZWMAAAAAugAAAAAAAABSAAAABgAAAGhl
YWRlcroAAAAAAAAAUgAAAAMAAAB0b3ABAAAAmgEAAAAAAACaAAAAAAAAAMABAABSAAAACAAAAENo
ZWNrQm94YgAAABAAAAAAAAAA8AkAAGIAAAACAAAAggAAAAQAAAADIAFEAQAAAGAMAABGBAsAAgAA
AFZhbHVlSG9sZGVyAAAAAAAAAAAAAAAASgMAAAAAAABgAwAAugAAAAAAAABSAAAABQAAAG5ldmVy
IAAAAAAAAAAAAAAABwAAAAAAAAAAAAAAAAAAAGAMAAAAAAAAggAAAAgAAAB/A///AAAAAPIKAAAA
AAAAAAAAAAAAAAAAAAAAAgUAAAAAAADKAAAAAAAAANAAAABiAAAAAQAAAEIFAAAAAAAAYAUAAGIA
AAACAAAAkgUAAAAAAADTAAAANQAAAJIFAAAAAAAAOwIAACsAAABgDAAAAgYAAAAAAAByAAAALAAA
ACwAAAAAAAAAAQAAAP////////////////////9pAAAAGgAAAIYBAAAvAAAAygAAAAAAAADQAAAA
MAMAAEAGAAAAAAAAEwAAAAIMAAAAAAAAugAAAAAAAABSAAAABAAAAGl0ZW1ADAAAAQAAAJoBAAAA
AAAAkAoAAGIAAAAQAAAAAAAAAPAJAABiAAAAAgAAAIIAAAAEAAAAAAEARAEABAAADgAAAAAAAAAA
AAAAAAAABwAAAAAAAAAAAAAAAAAAAAAOAAAAAAAAggAAAAgAAAB5A///AAAAAPIKAAAAAAAAAAAA
AAAAAAAAAAAAAgUAAAAAAADKAAAAAAAAANAAAABiAAAAAQAAAEIFAAAAAAAAYAUAAGIAAAACAAAA
kgUAAAAAAADTAAAAaQAAAJIFAAAAAAAAOwIAACsAAAAADgAAAgYAAAAAAAByAAAALAAAACwAAAAA
AAAAAQAAAP////////////////////9pAAAANAAAAIYBAABJAAAAygAAAAAAAADQAAAAMAMAAEAG
AAAAAAAAEwAAAAIMAAAAAAAA4A0AAEAMAAABAAAAmgEAAAAAAACQCgAAYgAAABAAAAAAAAAA8AkA
AGIAAAACAAAAggAAAAQAAAAAAQBEAQAAABAPAAAAAAAAAAAAAAAAAAAHAAAAAAAAAAAAAAAAAAAA
EA8AAAAAAACCAAAACAAAAHkD//8AAAAA8goAAAAAAAAAAAAAAAAAAAAAAAACBQAAAAAAAMoAAAAA
AAAA0AAAAGIAAAACAAAAQgUAAAAAAABgBQAAYgAAAAIAAACSBQAAAAAAACkAAABpAAAAkgUAAAAA
AAChAAAAKwAAABAPAABCBQAAAAAAAJALAABiAAAAAQAAAFIAAAAGAAAAUGF0aA0KEA8AAAIGAAAA
AAAAcgAAACwAAAAsAAAAAAAAAAEAAAD/////////////////////FAAAADQAAABkAAAASQAAAMoA
AAAAAAAA0AAAADADAABABgAAAAAAABMAAAACDAAAAAAAALoAAAAAAAAAUgAAAAUAAABsYWJlbEAM
AAABAAAAmgEAAAAAAACQCgAAYgAAABAAAAAAAAAA8AkAAGIAAAACAAAAggAAAAQAAAAAAQBEAQAE
AHAQAAAAAAAAAAAAAAAAAAAHAAAAAAAAAAAAAAAAAAAAcBAAAAAAAACCAAAACAAAAHkD//8AAAAA
8goAAAAAAAAAAAAAAAAAAAAAAAACBQAAAAAAAMoAAAAAAAAA0AAAAGIAAAABAAAAQgUAAAAAAABg
BQAAYgAAAAIAAACSBQAAAAAAANMAAACdAAAAkgUAAAAAAAA7AgAAKwAAAHAQAAACBgAAAAAAAHIA
AAAsAAAALAAAAAAAAAABAAAA/////////////////////2kAAABOAAAAhgEAAGMAAADKAAAAAAAA
ANAAAAAwAwAAQAYAAAAAAAATAAAAAgwAAAAAAADgDQAAQAwAAAEAAACaAQAAAAAAAJAKAABiAAAA
EAAAAAAAAADwCQAAYgAAAAIAAACCAAAABAAAAAABAEQBAAAAgBEAAAAAAAAAAAAAAAAAAAcAAAAA
AAAAAAAAAAAAAACAEQAAAAAAAIIAAAAIAAAAeQP//wAAAADyCgAAAAAAAAAAAAAAAAAAAAAAAAIF
AAAAAAAAygAAAAAAAADQAAAAYgAAAAIAAABCBQAAAAAAAGAFAABiAAAAAgAAAJIFAAAAAAAAKQAA
ADUAAACSBQAAAAAAAKEAAAArAAAAgBEAAEIFAAAAAAAAkAsAAGIAAAABAAAAUgAAAAcAAABFbmFi
bGVkgBEAAAIGAAAAAAAAcgAAACwAAAAsAAAAAAAAAAEAAAD/////////////////////FAAAABoA
AABkAAAALwAAAMoAAAAAAAAA0AAAADADAABABgAAAAAAABMAAAACDAAAAAAAAFAQAABADAAAAQAA
AJoBAAAAAAAAkAoAAGIAAAAQAAAAAAAAAPAJAABiAAAAAgAAAIIAAAAEAAAAAAEARAEAAADAEgAA
AAAAAAAAAAAAAAAABwAAAAAAAAAAAAAAAAAAAMASAAAAAAAAggAAAAgAAAB5A///AAAAAPIKAAAA
AAAAAAAAAAAAAAAAAAAAAgUAAAAAAADKAAAAAAAAANAAAABiAAAAAgAAAEIFAAAAAAAAYAUAAGIA
AAACAAAAkgUAAAAAAAApAAAAnQAAAJIFAAAAAAAAoQAAACsAAADAEgAAQgUAAAAAAACQCwAAYgAA
AAEAAABSAAAABQAAAENsYXNzwBIAAAIGAAAAAAAAcgAAACwAAAAsAAAAAAAAAAEAAAD/////////
////////////FAAAAE4AAABkAAAAYwAAAMoAAAAAAAAA0AAAADADAABABgAAAAAAABMAAAACDAAA
AAAAAFAQAABADAAAAQAAAOoAAAAAAAAAAAEAAGIAAAAIAAAAgAoAAFIAAAAPAAAAQ2xhc3Nsb2Fk
ZXJOYW1lYAwAAFIAAAASAAAAQ2xhc3Nsb2FkZXJFbmFibGVkAA4AAFIAAAAPAAAAQ2xhc3Nsb2Fk
ZXJQYXRocBAAAFIAAAAQAAAAQ2xhc3Nsb2FkZXJDbGFzcwAAAAACBQAAAAAAAMoAAAAAAAAA0AAA
AGIAAAABAAAAQgUAAAAAAABgBQAAYgAAAAIAAACSBQAAAAAAAAEAAAABAAAAkgUAAAAAAAANAwAA
xwAAAPAJAAACBgAAAAAAAHIAAAAsAAAALAAAAAAAAAABAAAA/////////////////////wAAAAAA
AAAAhgEAAGMAAADKAAAAAAAAANAAAABiAAAABwAAAIAKAACAEQAAYAwAABAPAAAADgAAwBIAAHAQ
AABABgAAAAAAABMAAABABgAAAAAAABUAAABABgAAAAAAABMAAABGBQQAAwAAAEljb24AAAAAAAAA
ABAAAAAOAhEAU1RCU2luZ2xldG9uUHJveHkAAAAAmgAAAAAAAABSAAAABwAAAERvbHBoaW5SAAAA
GAAAAEltYWdlUmVsYXRpdmVGaWxlTG9jYXRvcroAAAAAAAAAUgAAAAcAAABjdXJyZW50UgAAABEA
AABDb250YWluZXJWaWV3Lmljbw4CHwBTVEJFeHRlcm5hbFJlc291cmNlTGlicmFyeVByb3h5AAAA
AFIAAAAQAAAAZG9scGhpbmRyMDA1LmRsbAAAAAA='))!

(ResourceIdentifier class: JVMDBPrintfPage name: 'Default view') assign: (Object fromBinaryStoreBytes:
(ByteArray fromBase64String: 'IVNUQiAxIEYCDAABAAAAVmlld1Jlc291cmNlAAAAAA4BJABTVEJSZXNvdXJjZVNUQkJ5dGVBcnJh
eUFjY2Vzc29yUHJveHkAAAAAcgAAALsKAAAhU1RCIDAgTggMAAoAAABTVEJWaWV3UHJveHkAAAAA
TgINAAEAAABTVEJDbGFzc1Byb3h5AAAAADYABgBTdHJpbmcHAAAARG9scGhpbpIAAAANAAAAQ29u
dGFpbmVyVmlldyYABQBBcnJheQ8AAAAAAAAAAAAAAMIAAAACAAAANgAMAExhcmdlSW50ZWdlcgQA
AAAAAABEAQACAGAAAAAAAAAABgELAFN5c3RlbUNvbG9yAAAAAB8AAAAAAAAABwAAAAAAAAAAAAAA
AAAAAAAAAAAGAQ0ARnJhbWluZ0xheW91dAAAAAAOAhoAU1RCSWRlbnRpdHlEaWN0aW9uYXJ5UHJv
eHkAAAAAegAAAAAAAACgAAAAkgAAAAsAAABMb29rdXBUYWJsZcIAAAAEAAAAWgAAAAAAAAB6AAAA
AAAAAKAAAACSAAAACgAAAFB1c2hCdXR0b27CAAAAEQAAAAAAAABgAAAAwgAAAAIAAADyAAAABAAA
AAAgAUQBAAAAoAEAAAAAAAAAAAAAAAAAAAcAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAPIAAAAEAAAA
brLhd0YEEgACAAAAQ29tbWFuZERlc2NyaXB0aW9uAAAAAA4BDgBTVEJTeW1ib2xQcm94eQAAAACS
AAAACgAAAGNsZWFyVHJhY2WSAAAABgAAACZDbGVhcgEAAAAAAAAAAAAAAAEAAAAGAQ8ATWVzc2Fn
ZVNlcXVlbmNlAAAAAA4CEgBTVEJDb2xsZWN0aW9uUHJveHkAAAAAegAAAAAAAACgAAAAkgAAABEA
AABPcmRlcmVkQ29sbGVjdGlvbsIAAAADAAAABgMLAE1lc3NhZ2VTZW5kAAAAADoCAAAAAAAAkgAA
ABAAAABjcmVhdGVBdDpleHRlbnQ6wgAAAAIAAAAGAgUAUG9pbnQAAAAAJwIAAKUBAAAyAwAAAAAA
AKEAAAAzAAAAoAEAAOICAAAAAAAAOgIAAAAAAACSAAAACgAAAGlzRW5hYmxlZDrCAAAAAQAAACAA
AACgAQAA4gIAAAAAAAA6AgAAAAAAAJIAAAAFAAAAdGV4dDrCAAAAAQAAAJIAAAAGAAAAJkNsZWFy
oAEAAAYBDwBXSU5ET1dQTEFDRU1FTlQAAAAANgAJAEJ5dGVBcnJheSwAAAAsAAAAAAAAAAEAAAD/
////////////////////EwEAANIAAABjAQAA6wAAAJoCAAAAAAAAsAIAAMIAAAAAAAAAMgMAAAAA
AADBAAAAwQAAAAAAAAATAAAARggSAAEAAABGcmFtaW5nQ29uc3RyYWludHMAAAAAOgIAAAAAAACS
AAAADgAAAGZpeGVkVmlld1JpZ2h0Yf///zoCAAAAAAAAkgAAABAAAABmaXhlZFBhcmVudFJpZ2h0
9////zoCAAAAAAAAkgAAABMAAABmaXhlZFByZXZpb3VzQm90dG9tCwAAADoCAAAAAAAAkgAAABEA
AABmaXhlZFBhcmVudEJvdHRvbff///9aAAAAAAAAAHoAAAAAAAAAoAAAAJIAAAAMAAAAUmljaFRl
eHRFZGl0wgAAABIAAAAAAAAAYAAAAMIAAAACAAAA8gAAAAQAAADEETFEAQAEAAAFAAAAAAAABgMD
AFJHQgAAAACtAQAArQEAAK0BAAAAAAAABwAAAEYFBAACAAAATWVudQAAAAAAAAAAEAAAAMIAAAAH
AAAARgIPAAEAAABDb21tYW5kTWVudUl0ZW0AAAAAAQAAABICAAAAAAAAOgIAAAAAAACSAAAADQAA
AHRvZ2dsZVRyYWNpbmeSAAAADQAAACZUcmFjZSBldmVudHMBAAAAAAAAAEYBDwABAAAARGl2aWRl
ck1lbnVJdGVtAAAAAAEQAACyBQAAAAAAAAEAAAASAgAAAAAAADoCAAAAAAAAkgAAAA0AAABjb3B5
U2VsZWN0aW9ukgAAAAUAAAAmQ29weYckAAAAAAAAsgUAAAAAAAABAAAAEgIAAAAAAAA6AgAAAAAA
AJIAAAAJAAAAc2VsZWN0QWxskgAAAAsAAABTZWxlY3QgJkFsbIMgAAAAAAAAsgUAAAAAAAABAAAA
EgIAAAAAAABAAgAAkgAAAAYAAABDbGVhJnIBAAAAAAAAABIGAAAAAAAAARAAALIFAAAAAAAAAQAA
ABICAAAAAAAAOgIAAAAAAACSAAAADgAAAHRvZ2dsZVdvcmRXcmFwkgAAAAoAAAAmV29yZCBXcmFw
AQAAAAAAAACSAAAABQAAACZFZGl0AAAAAAAAAAAAAAAAAAAAAAAAAADyAAAABAAAABsRLncGAg0A
TnVsbENvbnZlcnRlcgAAAAAAAAAAAAAAAAsAAAAAAAAABgEKAEVESVRTVFJFQU0AAAAAEgQAAAwA
AAAAAAAAAAAAANSVFAlyAgAAAAAAAJoCAAAAAAAAsAIAAMIAAAAGAAAA4gIAAAAAAAAAAwAAwgAA
AAIAAAAyAwAAAAAAAAsAAAALAAAAMgMAAAAAAAC9AgAAkQEAAAAFAADiAgAAAAAAADoCAAAAAAAA
kgAAAAwAAABjb250ZXh0TWVudTrCAAAAAQAAAJAFAAAABQAA4gIAAAAAAACwAwAAwgAAAAEAAAAG
AQgAUmljaFRleHQAAAAAkgAAAH4AAAB7XHJ0ZjFcYW5zaVxhbnNpY3BnMTI1MlxkZWZmMFxkZWZs
YW5nMjA1N3tcZm9udHRibHtcZjBcZnJvbWFuXGZwcnEyIFRpbWVzIE5ldyBSb21hbjt9fQ0KXHZp
ZXdraW5kNFx1YzFccGFyZFxmMFxmczIyIA0KXHBhciB9DQoABQAA4gIAAAAAAAA6AgAAAAAAAJIA
AAAPAAAAc2VsZWN0aW9uUmFuZ2U6wgAAAAEAAAAGAwgASW50ZXJ2YWwAAAAAAwAAAAEAAAADAAAA
AAUAAOICAAAAAAAAOgIAAAAAAACSAAAADwAAAGlzVGV4dE1vZGlmaWVkOsIAAAABAAAAIAAAAAAF
AADiAgAAAAAAADoCAAAAAAAAkgAAAA8AAAByZXNldENoYXJGb3JtYXTCAAAAAAAAAAAFAADyAwAA
AAAAABIEAAAsAAAALAAAAAAAAAABAAAA/////////////////////wUAAAAFAAAAYwEAAM0AAACa
AgAAAAAAALACAABABAAAUAQAAAAAAAATAAAAYgQAAAAAAAA6AgAAAAAAAJIAAAAPAAAAZml4ZWRQ
YXJlbnRMZWZ0CwAAAKAEAAD3////OgIAAAAAAACSAAAADgAAAGZpeGVkUGFyZW50VG9wCwAAAOAE
AAC7////WgEAAAAAAAB6AAAAAAAAAKAAAACSAAAAEgAAAElkZW50aXR5RGljdGlvbmFyecIAAAAC
AAAAAAUAAJIAAAAFAAAAVHJhY2UAAAAAcgIAAAAAAACaAgAAAAAAALACAADCAAAAAQAAAOICAAAA
AAAAAAMAAMIAAAACAAAAMgMAAAAAAAALAAAACwAAADIDAAAAAAAA0QIAAOEBAABgAAAA8gMAAAAA
AAASBAAALAAAACwAAAAAAAAAAAAAAP////////////////////8FAAAABQAAAG0BAAD1AAAAmgIA
AAAAAACwAgAAwgAAAAIAAAAABQAAoAEAAFAEAAAAAAAAEwAAAEYFBAADAAAASWNvbgAAAAAAAAAA
EAAAAA4CEQBTVEJTaW5nbGV0b25Qcm94eQAAAACaAAAAAAAAAFIAAAAHAAAARG9scGhpblIAAAAY
AAAASW1hZ2VSZWxhdGl2ZUZpbGVMb2NhdG9yugAAAAAAAABSAAAABwAAAGN1cnJlbnRSAAAAEQAA
AENvbnRhaW5lclZpZXcuaWNvDgIfAFNUQkV4dGVybmFsUmVzb3VyY2VMaWJyYXJ5UHJveHkAAAAA
UgAAABAAAABkb2xwaGluZHIwMDUuZGxsAAAAAA=='))!

(ResourceIdentifier class: JVMStatusPage name: 'Default view') assign: (Object fromBinaryStoreBytes:
(ByteArray fromBase64String: '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
AAC5AQAAEgYAAAAAAACrAAAAMwAAAIAHAADCBQAAAAAAALoAAAAAAAAAUgAAAAoAAABpc0VuYWJs
ZWQ6YgAAAAEAAAAgAAAAgAcAAMIFAAAAAAAAUAYAAGIAAAABAAAAUgAAAAkAAABTaHV0ZG93biGA
BwAAkgYAAAAAAAByAAAALAAAACwAAAAAAAAAAQAAAP////////////////////8EAQAA3AAAAFkB
AAD1AAAAygAAAAAAAADQAAAAEAMAANAGAAAAAAAAEwAAAOIGAAAAAAAAugAAAAAAAABSAAAADgAA
AGZpeGVkVmlld1JpZ2h0V////7oAAAAAAAAAUgAAABIAAABmaXhlZFByZXZpb3VzUmlnaHQBAAAA
ugAAAAAAAABSAAAAEwAAAGZpeGVkUHJldmlvdXNCb3R0b20LAAAAYAcAAPf////qAAAAAAAAAAAB
AABiAAAAAgAAAHACAABSAAAACgAAAFN0YXR1c0xpc3QAAAAAggUAAAAAAADKAAAAAAAAANAAAABi
AAAAAQAAAMIFAAAAAAAA4AUAAGIAAAACAAAAEgYAAAAAAAALAAAACwAAABIGAAAAAAAAvQIAAPUB
AACgAQAAkgYAAAAAAAByAAAALAAAACwAAAAAAAAAAAAAAP////////////////////8FAAAABQAA
AGMBAAD/AAAAygAAAAAAAADQAAAAYgAAAAIAAABwAgAAgAcAANAGAAAAAAAAEwAAAEYFBAADAAAA
SWNvbgAAAAAAAAAAEAAAAA4CEQBTVEJTaW5nbGV0b25Qcm94eQAAAACaAAAAAAAAAFIAAAAHAAAA
RG9scGhpblIAAAAYAAAASW1hZ2VSZWxhdGl2ZUZpbGVMb2NhdG9yugAAAAAAAABSAAAABwAAAGN1
cnJlbnRSAAAAEQAAAENvbnRhaW5lclZpZXcuaWNvDgIfAFNUQkV4dGVybmFsUmVzb3VyY2VMaWJy
YXJ5UHJveHkAAAAAUgAAABAAAABkb2xwaGluZHIwMDUuZGxsAAAAAA=='))!

