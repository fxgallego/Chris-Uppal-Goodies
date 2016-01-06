| package |
package := Package name: 'CU JNIPort'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2003.
chris.uppal@metagnostic.org

This package is empty except for a list of prerequisites that will load all of JNIPort
(except the tests and examples).  You can uninstall this package if you want, it will leave JNIPort installed.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris'.

package basicPackageVersion: '1.00'.


package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'CU Java Additional Wrappers';
	add: 'CU Java Console Page';
	add: 'CU Java Ghost Classes';
	add: 'CU Java History Page';
	add: 'CU Java Wrapper Wizard';
	add: 'CU JNI Helper';
	yourself).

package setManualPrerequisites: #(
	'CU Java Additional Wrappers'
	'CU Java Console Page'
	'CU Java Console Page'
	'CU Java Ghost Classes'
	'CU Java History Page'
	'CU Java Wrapper Wizard'
	'CU JNI Helper').

package!

"Class Definitions"!


"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

"Binary Globals"!

"Resources"!

