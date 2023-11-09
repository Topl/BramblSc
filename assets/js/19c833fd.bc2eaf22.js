"use strict";(self.webpackChunkbramblsc_documentation=self.webpackChunkbramblsc_documentation||[]).push([[657],{3905:(e,t,n)=>{n.d(t,{Zo:()=>c,kt:()=>f});var r=n(7294);function a(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function l(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);t&&(r=r.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,r)}return n}function i(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?l(Object(n),!0).forEach((function(t){a(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):l(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function o(e,t){if(null==e)return{};var n,r,a=function(e,t){if(null==e)return{};var n,r,a={},l=Object.keys(e);for(r=0;r<l.length;r++)n=l[r],t.indexOf(n)>=0||(a[n]=e[n]);return a}(e,t);if(Object.getOwnPropertySymbols){var l=Object.getOwnPropertySymbols(e);for(r=0;r<l.length;r++)n=l[r],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(a[n]=e[n])}return a}var p=r.createContext({}),s=function(e){var t=r.useContext(p),n=t;return e&&(n="function"==typeof e?e(t):i(i({},t),e)),n},c=function(e){var t=s(e.components);return r.createElement(p.Provider,{value:t},e.children)},m="mdxType",u={inlineCode:"code",wrapper:function(e){var t=e.children;return r.createElement(r.Fragment,{},t)}},d=r.forwardRef((function(e,t){var n=e.components,a=e.mdxType,l=e.originalType,p=e.parentName,c=o(e,["components","mdxType","originalType","parentName"]),m=s(n),d=a,f=m["".concat(p,".").concat(d)]||m[d]||u[d]||l;return n?r.createElement(f,i(i({ref:t},c),{},{components:n})):r.createElement(f,i({ref:t},c))}));function f(e,t){var n=arguments,a=t&&t.mdxType;if("string"==typeof e||a){var l=n.length,i=new Array(l);i[0]=d;var o={};for(var p in t)hasOwnProperty.call(t,p)&&(o[p]=t[p]);o.originalType=e,o[m]="string"==typeof e?e:a,i[1]=o;for(var s=2;s<l;s++)i[s]=n[s];return r.createElement.apply(null,i)}return r.createElement.apply(null,n)}d.displayName="MDXCreateElement"},7884:(e,t,n)=>{n.d(t,{Z:()=>o});var r=n(7294),a=n(9960),l=n(6550),i=n(8084);function o(e){let{children:t,path:n}=e;var o=function(){const{pathname:e}=(0,l.TH)(),{versions:t}=(0,i.eZ)("docusaurus-plugin-content-docs");return t.find((t=>{let{path:n}=t;return e.startsWith(n)})).name||"current"}();return r.createElement(a.Z,{to:`/scaladoc/${o}/${n}`,target:"_blank"},t)}},3998:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>s,contentTitle:()=>o,default:()=>d,frontMatter:()=>i,metadata:()=>p,toc:()=>c});var r=n(7462),a=(n(7294),n(3905)),l=n(7884);const i={sidebar_position:2,title:"Import a Wallet",description:"Import an existing Wallet"},o=void 0,p={unversionedId:"reference/wallets/import",id:"reference/wallets/import",title:"Import a Wallet",description:"Import an existing Wallet",source:"@site/docs/reference/wallets/import.mdx",sourceDirName:"reference/wallets",slug:"/reference/wallets/import",permalink:"/BramblSc/docs/current/reference/wallets/import",draft:!1,tags:[],version:"current",sidebarPosition:2,frontMatter:{sidebar_position:2,title:"Import a Wallet",description:"Import an existing Wallet"},sidebar:"referenceSidebar",previous:{title:"Create a Wallet",permalink:"/BramblSc/docs/current/reference/wallets/create"},next:{title:"RPC",permalink:"/BramblSc/docs/current/reference/rpc"}},s={},c=[{value:"Example",id:"example",level:2}],m={toc:c},u="wrapper";function d(e){let{components:t,...n}=e;return(0,a.kt)(u,(0,r.Z)({},m,n,{components:t,mdxType:"MDXLayout"}),(0,a.kt)("p",null,'Importing a wallet involves generating your existing Topl main key pair using your passphrase and your previously-derived mnemonic.\nThis is also referred to as "Recovering a Wallet". If you do not have an existing wallet to import (or recover), see\n',(0,a.kt)("a",{parentName:"p",href:"./create"},"Create a Wallet"),". You can import your wallet using\nthe ",(0,a.kt)(l.Z,{path:"co/topl/brambl/wallet/WalletApi.html#importWallet(IndexedSeq[String],Array[Byte],Option[String]):F[Either[WalletApiFailure,VaultStore[F]]]",mdxType:"ScaladocLink"},(0,a.kt)("code",null,"importWallet")),"\nfunction of a Wallet API instance."),(0,a.kt)("pre",null,(0,a.kt)("code",{parentName:"pre",className:"language-scala"},"def importWallet(\n  mnemonic:   IndexedSeq[String],\n  password:   Array[Byte],\n  passphrase: Option[String] = None\n): F[Either[WalletApi.WalletApiFailure, VaultStore[F]]]\n")),(0,a.kt)("p",null,"This function generates the Topl main key pair associated to the mnemonic and passphrase, and encrypts it with the provided password."),(0,a.kt)("p",null,"The parameters are as follows:"),(0,a.kt)("ul",null,(0,a.kt)("li",{parentName:"ul"},(0,a.kt)("inlineCode",{parentName:"li"},"mnemonic"),": The mnemonic used to regenerate the existing Topl main key pair"),(0,a.kt)("li",{parentName:"ul"},(0,a.kt)("inlineCode",{parentName:"li"},"password"),": The password to encrypt the generated Topl main key pair with"),(0,a.kt)("li",{parentName:"ul"},(0,a.kt)("inlineCode",{parentName:"li"},"passphrase"),": An optional passphrase used to regenerate the existing Topl main key pair. The default is no passphrase.")),(0,a.kt)("p",null,"On success, this function returns the regenerated Topl main key pair encrypted into a ",(0,a.kt)("inlineCode",{parentName:"p"},"VaultStore")," instance. On failure, this\nfunction returns\na ",(0,a.kt)(l.Z,{path:"co/topl/brambl/wallet/WalletApi$$WalletApiFailure.html",mdxType:"ScaladocLink"},(0,a.kt)("code",null,"WalletApiFailure")),"\nwhich specifies the reason for failure."),(0,a.kt)("admonition",{type:"note"},(0,a.kt)("p",{parentName:"admonition"},"The provided mnemonic and passphrase ",(0,a.kt)("strong",{parentName:"p"},"must")," be the same as the mnemonic and passphrase used to generate the original Topl main key pair.\nThe password could be different.")),(0,a.kt)("h2",{id:"example"},"Example"),(0,a.kt)("p",null,"The following example shows how to import an existing wallet using a mnemonic. This example assumes that no passphrase\nwas used in the initial creation of the existing wallet."),(0,a.kt)("pre",null,(0,a.kt)("code",{parentName:"pre",className:"language-scala"},'import cats.effect.IO\nimport co.topl.brambl.wallet.WalletApi\nimport co.topl.brambl.servicekit.WalletKeyApi\n\nval walletApi = WalletApi.make[IO](WalletKeyApi.make())\n\n// Some mock mnemonic. Replace with your own.\nval someMnemonic = "ozone drill grab fiber curtain grace pudding thank cruise elder eight picnic"\n\nval recoverWalletResult = walletApi.importWallet(someMnemonic.split(" "), "password".getBytes)\n')))}d.isMDXComponent=!0}}]);