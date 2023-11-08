"use strict";(self.webpackChunkbramblsc_documentation=self.webpackChunkbramblsc_documentation||[]).push([[991],{3905:(e,t,a)=>{a.d(t,{Zo:()=>c,kt:()=>k});var o=a(7294);function n(e,t,a){return t in e?Object.defineProperty(e,t,{value:a,enumerable:!0,configurable:!0,writable:!0}):e[t]=a,e}function l(e,t){var a=Object.keys(e);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);t&&(o=o.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),a.push.apply(a,o)}return a}function r(e){for(var t=1;t<arguments.length;t++){var a=null!=arguments[t]?arguments[t]:{};t%2?l(Object(a),!0).forEach((function(t){n(e,t,a[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(a)):l(Object(a)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(a,t))}))}return e}function i(e,t){if(null==e)return{};var a,o,n=function(e,t){if(null==e)return{};var a,o,n={},l=Object.keys(e);for(o=0;o<l.length;o++)a=l[o],t.indexOf(a)>=0||(n[a]=e[a]);return n}(e,t);if(Object.getOwnPropertySymbols){var l=Object.getOwnPropertySymbols(e);for(o=0;o<l.length;o++)a=l[o],t.indexOf(a)>=0||Object.prototype.propertyIsEnumerable.call(e,a)&&(n[a]=e[a])}return n}var p=o.createContext({}),s=function(e){var t=o.useContext(p),a=t;return e&&(a="function"==typeof e?e(t):r(r({},t),e)),a},c=function(e){var t=s(e.components);return o.createElement(p.Provider,{value:t},e.children)},m="mdxType",d={inlineCode:"code",wrapper:function(e){var t=e.children;return o.createElement(o.Fragment,{},t)}},u=o.forwardRef((function(e,t){var a=e.components,n=e.mdxType,l=e.originalType,p=e.parentName,c=i(e,["components","mdxType","originalType","parentName"]),m=s(a),u=n,k=m["".concat(p,".").concat(u)]||m[u]||d[u]||l;return a?o.createElement(k,r(r({ref:t},c),{},{components:a})):o.createElement(k,r({ref:t},c))}));function k(e,t){var a=arguments,n=t&&t.mdxType;if("string"==typeof e||n){var l=a.length,r=new Array(l);r[0]=u;var i={};for(var p in t)hasOwnProperty.call(t,p)&&(i[p]=t[p]);i.originalType=e,i[m]="string"==typeof e?e:n,r[1]=i;for(var s=2;s<l;s++)r[s]=a[s];return o.createElement.apply(null,r)}return o.createElement.apply(null,a)}u.displayName="MDXCreateElement"},7884:(e,t,a)=>{a.d(t,{Z:()=>i});var o=a(7294),n=a(9960),l=a(6550),r=a(8084);function i(e){let{children:t,path:a}=e;var i=function(){const{pathname:e}=(0,l.TH)(),{versions:t}=(0,r.eZ)("docusaurus-plugin-content-docs");return t.find((t=>{let{path:a}=t;return e.startsWith(a)})).name||"current"}();return o.createElement(n.Z,{to:`/scaladoc/${i}/${a}`,target:"_blank"},t)}},6890:(e,t,a)=>{a.r(t),a.d(t,{assets:()=>c,contentTitle:()=>p,default:()=>k,frontMatter:()=>i,metadata:()=>s,toc:()=>m});var o=a(7462),n=(a(7294),a(3905)),l=a(9960),r=a(7884);const i={sidebar_position:1,title:"Create a Lock Template",description:"Create a template used for building complex Locks."},p=void 0,s={unversionedId:"reference/locks/create-template",id:"reference/locks/create-template",title:"Create a Lock Template",description:"Create a template used for building complex Locks.",source:"@site/docs/reference/locks/create-template.mdx",sourceDirName:"reference/locks",slug:"/reference/locks/create-template",permalink:"/BramblSc/docs/current/reference/locks/create-template",draft:!1,tags:[],version:"current",sidebarPosition:1,frontMatter:{sidebar_position:1,title:"Create a Lock Template",description:"Create a template used for building complex Locks."},sidebar:"referenceSidebar",previous:{title:"Locks",permalink:"/BramblSc/docs/current/reference/locks/"},next:{title:"Generate a Lock",permalink:"/BramblSc/docs/current/reference/locks/build-lock"}},c={},m=[{value:"Create a Predicate Lock Template",id:"create-a-predicate-lock-template",level:2},{value:"Example",id:"example",level:3},{value:"Create a Proposition Template",id:"create-a-proposition-template",level:2},{value:"LockedTemplate",id:"lockedtemplate",level:3},{value:"HeightTemplate",id:"heighttemplate",level:3},{value:"TickTemplate",id:"ticktemplate",level:3},{value:"DigestTemplate",id:"digesttemplate",level:3},{value:"SignatureTemplate",id:"signaturetemplate",level:3},{value:"AndTemplate",id:"andtemplate",level:3},{value:"OrTemplate",id:"ortemplate",level:3},{value:"NotTemplate",id:"nottemplate",level:3},{value:"ThresholdTemplate",id:"thresholdtemplate",level:3}],d={toc:m},u="wrapper";function k(e){let{components:t,...a}=e;return(0,n.kt)(u,(0,o.Z)({},d,a,{components:t,mdxType:"MDXLayout"}),(0,n.kt)("p",null,"Creating Lock Templates is the first step to creating a new Lock and it's corresponding LockAddress."),(0,n.kt)("h2",{id:"create-a-predicate-lock-template"},"Create a Predicate Lock Template"),(0,n.kt)("p",null,"You can create a Predicate Lock template using\nthe ",(0,n.kt)(r.Z,{path:"co/topl/brambl/builders/locks/LockTemplate$$PredicateTemplate.html",mdxType:"ScaladocLink"},(0,n.kt)("code",null,"PredicateTemplate")),"\ncase class:"),(0,n.kt)("pre",null,(0,n.kt)("code",{parentName:"pre",className:"language-scala"},"case class PredicateTemplate[F[_]: Monad](\n  innerTemplates: Seq[PropositionTemplate[F]],\n  threshold: Int\n) extends LockTemplate[F]\n")),(0,n.kt)("p",null,"The parameters are as follows:"),(0,n.kt)("ul",null,(0,n.kt)("li",{parentName:"ul"},(0,n.kt)("inlineCode",{parentName:"li"},"innerTemplates"),": A sequence of ",(0,n.kt)("a",{parentName:"li",href:"#create-a-proposition-template"},"Proposition Templates")," that will be used to create the inner propositions (Challenges)\nof the Predicate Lock."),(0,n.kt)("li",{parentName:"ul"},(0,n.kt)("inlineCode",{parentName:"li"},"threshold"),": The number of inner propositions that must be satisfied in order to satisfy the Predicate Lock.")),(0,n.kt)("p",null,"Type parameters:"),(0,n.kt)("ul",null,(0,n.kt)("li",{parentName:"ul"},(0,n.kt)("inlineCode",{parentName:"li"},"F"),": The context, bound to a context parameter of type ",(0,n.kt)("inlineCode",{parentName:"li"},"Monad[F]"),", in which the template will be created.")),(0,n.kt)("p",null,"The resulting ",(0,n.kt)("inlineCode",{parentName:"p"},"PredicateTemplate")," can be used to generate a Lock."),(0,n.kt)("h3",{id:"example"},"Example"),(0,n.kt)("p",null,"The following example shows how to create a Template for a 1-of-1 Signature Lock with a Cats Effect ",(0,n.kt)("inlineCode",{parentName:"p"},"IO")," context."),(0,n.kt)("pre",null,(0,n.kt)("code",{parentName:"pre",className:"language-scala"},'import cats.effect.IO\nimport co.topl.brambl.builders.locks.{LockTemplate, PropositionTemplate}\nimport co.topl.brambl.builders.locks.PropositionTemplate.SignatureTemplate\nimport co.topl.brambl.builders.locks.LockTemplate.PredicateTemplate\n\n\nval signatureTemplate: PropositionTemplate[IO] = SignatureTemplate[IO]("ExtendedEd25519", 0)\nval predicateTemplate: LockTemplate[IO] = PredicateTemplate[IO](Seq(signatureTemplate), 1)\n')),(0,n.kt)("h2",{id:"create-a-proposition-template"},"Create a Proposition Template"),(0,n.kt)("p",null,"Creating a Lock Template requires the inner Proposition Templates to be created first. You can create proposition\ntemplates using the various proposition template case classes in\nthe ",(0,n.kt)(r.Z,{path:"co/topl/brambl/builders/locks/PropositionTemplate$.html",mdxType:"ScaladocLink"},(0,n.kt)("code",null,"PropositionTemplate object")),".\nOnce created, the resulting PropositionTemplates can be used to create a Predicate Lock Template (",(0,n.kt)("a",{parentName:"p",href:"#create-a-predicate-lock-template"},"as the ",(0,n.kt)("inlineCode",{parentName:"a"},"innerTemplates"),"\nparameter"),")."),(0,n.kt)("p",null,"All Proposition Template case classes have a type parameter ",(0,n.kt)("inlineCode",{parentName:"p"},"F"),", bound to a context parameter of type ",(0,n.kt)("inlineCode",{parentName:"p"},"Monad[F]"),", in which\nthe template will be created. This should be the same as the type parameter used for the ",(0,n.kt)("inlineCode",{parentName:"p"},"PredicateTemplate"),"."),(0,n.kt)("p",null,"We provide the following Proposition Templates:"),(0,n.kt)("h3",{id:"lockedtemplate"},"LockedTemplate"),(0,n.kt)("p",null,"Once built, a LockedTemplate refers to\na ",(0,n.kt)(l.Z,{href:"https://github.com/Topl/protobuf-specs/blob/main/proto/quivr/models/proposition.proto#L30",mdxType:"Link"},"Locked proposition"),".\nYou can create a LockedTemplate using\nthe ",(0,n.kt)(r.Z,{path:"co/topl/brambl/builders/locks/PropositionTemplate$$LockedTemplate.html",mdxType:"ScaladocLink"},(0,n.kt)("code",null,"LockedTemplate")),"\ncase class:"),(0,n.kt)("pre",null,(0,n.kt)("code",{parentName:"pre",className:"language-scala"},"case class LockedTemplate[F[_]: Monad](\n  data: Option[Data]\n) extends PropositionTemplate[F]\n")),(0,n.kt)("p",null,"The parameters are the same as a Locked proposition."),(0,n.kt)("h3",{id:"heighttemplate"},"HeightTemplate"),(0,n.kt)("p",null,"Once built, a HeightTemplate refers to\na ",(0,n.kt)(l.Z,{href:"https://github.com/Topl/protobuf-specs/blob/main/proto/quivr/models/proposition.proto#L48",mdxType:"Link"},"HeightRange proposition"),".\nYou can create a HeightTemplate using\nthe ",(0,n.kt)(r.Z,{path:"co/topl/brambl/builders/locks/PropositionTemplate$$HeightTemplate.html",mdxType:"ScaladocLink"},(0,n.kt)("code",null,"HeightTemplate")),"\ncase class:"),(0,n.kt)("pre",null,(0,n.kt)("code",{parentName:"pre",className:"language-scala"},"case class HeightTemplate[F[_]: Monad](\n  chain: String,\n  min: Long,\n  max: Long\n) extends PropositionTemplate[F]\n")),(0,n.kt)("p",null,"The parameters are the same as a HeightRange proposition."),(0,n.kt)("h3",{id:"ticktemplate"},"TickTemplate"),(0,n.kt)("p",null,"Once built, a TickTemplate refers to\na ",(0,n.kt)(l.Z,{href:"https://github.com/Topl/protobuf-specs/blob/main/proto/quivr/models/proposition.proto#L55",mdxType:"Link"},"TickRange proposition"),".\nYou can create a TickTemplate using\nthe ",(0,n.kt)(r.Z,{path:"co/topl/brambl/builders/locks/PropositionTemplate$$TickTemplate.html",mdxType:"ScaladocLink"},(0,n.kt)("code",null,"TickTemplate")),"\ncase class:"),(0,n.kt)("pre",null,(0,n.kt)("code",{parentName:"pre",className:"language-scala"},"case class TickTemplate[F[_]: Monad](\n  min: Long,\n  max: Long\n) extends PropositionTemplate[F]\n")),(0,n.kt)("p",null,"The parameters are the same as a TickRange proposition."),(0,n.kt)("h3",{id:"digesttemplate"},"DigestTemplate"),(0,n.kt)("p",null,"Once built, a DigestTemplate refers to\na ",(0,n.kt)(l.Z,{href:"https://github.com/Topl/protobuf-specs/blob/main/proto/quivr/models/proposition.proto#L36",mdxType:"Link"},"Digest proposition"),".\nYou can create a DigestTemplate using\nthe ",(0,n.kt)(r.Z,{path:"co/topl/brambl/builders/locks/PropositionTemplate$$DigestTemplate.html",mdxType:"ScaladocLink"},(0,n.kt)("code",null,"DigestTemplate")),"\ncase class:"),(0,n.kt)("pre",null,(0,n.kt)("code",{parentName:"pre",className:"language-scala"},"case class DigestTemplate[F[_]: Monad](\n  routine: String,\n  digest: Digest\n) extends PropositionTemplate[F]\n")),(0,n.kt)("p",null,"The parameters are the same as a Digest proposition."),(0,n.kt)("h3",{id:"signaturetemplate"},"SignatureTemplate"),(0,n.kt)("p",null,"Once built, a SignatureTemplate refers to\na ",(0,n.kt)(l.Z,{href:"https://github.com/Topl/protobuf-specs/blob/main/proto/quivr/models/proposition.proto#L42",mdxType:"Link"},"DigitalSignature proposition"),".\nYou can create a SignatureTemplate using\nthe ",(0,n.kt)(r.Z,{path:"co/topl/brambl/builders/locks/PropositionTemplate$$SignatureTemplate.html",mdxType:"ScaladocLink"},(0,n.kt)("code",null,"SignatureTemplate")),"\ncase class:"),(0,n.kt)("pre",null,(0,n.kt)("code",{parentName:"pre",className:"language-scala"},"case class SignatureTemplate[F[_]: Monad](\n  routine: String,\n  entityIdx: Int\n) extends PropositionTemplate[F]\n")),(0,n.kt)("p",null,"The parameters are similar to a DigitalSignature proposition. The key difference being that, instead of specifying the\n",(0,n.kt)("inlineCode",{parentName:"p"},"verificationKey")," field, we specify an ",(0,n.kt)("inlineCode",{parentName:"p"},"entityIdx"),". This ",(0,n.kt)("inlineCode",{parentName:"p"},"entityIdx")," represents the index of an entity's VerificationKey\nin a list of verification keys. ",(0,n.kt)("inlineCode",{parentName:"p"},"entityIdx")," in conjunction with this list of verification keys (which is provided when building\nthe lock), will be used to populate the ",(0,n.kt)("inlineCode",{parentName:"p"},"verificationKey")," field in the built DigitalSignature proposition.\nSee ",(0,n.kt)("a",{parentName:"p",href:"./build-lock"},"Generate a Lock")," for more information."),(0,n.kt)("h3",{id:"andtemplate"},"AndTemplate"),(0,n.kt)("p",null,"Once built, an AndTemplate refers to\nan ",(0,n.kt)(l.Z,{href:"https://github.com/Topl/protobuf-specs/blob/main/proto/quivr/models/proposition.proto#L96",mdxType:"Link"},"And proposition"),".\nYou can create an AndTemplate using\nthe ",(0,n.kt)(r.Z,{path:"co/topl/brambl/builders/locks/PropositionTemplate$$AndTemplate.html",mdxType:"ScaladocLink"},(0,n.kt)("code",null,"AndTemplate")),"\ncase class:"),(0,n.kt)("pre",null,(0,n.kt)("code",{parentName:"pre",className:"language-scala"},"case class AndTemplate[F[_]: Monad](\n  leftTemplate: PropositionTemplate[F],\n  rightTemplate: PropositionTemplate[F]\n) extends PropositionTemplate[F]\n")),(0,n.kt)("p",null,"The parameters are similar to an And proposition. The key difference being that, instead of specifying the ",(0,n.kt)("inlineCode",{parentName:"p"},"left")," and\n",(0,n.kt)("inlineCode",{parentName:"p"},"right")," fields as propositions directly, we specify them as Proposition Templates."),(0,n.kt)("h3",{id:"ortemplate"},"OrTemplate"),(0,n.kt)("p",null,"Once built, an OrTemplate refers to\nan ",(0,n.kt)(l.Z,{href:"https://github.com/Topl/protobuf-specs/blob/main/proto/quivr/models/proposition.proto#L102",mdxType:"Link"},"Or proposition"),".\nYou can create an OrTemplate using\nthe ",(0,n.kt)(r.Z,{path:"co/topl/brambl/builders/locks/PropositionTemplate$$OrTemplate.html",mdxType:"ScaladocLink"},(0,n.kt)("code",null,"OrTemplate")),"\ncase class:"),(0,n.kt)("pre",null,(0,n.kt)("code",{parentName:"pre",className:"language-scala"},"case class OrTemplate[F[_]: Monad](\n  leftTemplate: PropositionTemplate[F],\n  rightTemplate: PropositionTemplate[F]\n) extends PropositionTemplate[F]\n")),(0,n.kt)("p",null,"The parameters are similar to an Or proposition. The key difference being that, instead of specifying the ",(0,n.kt)("inlineCode",{parentName:"p"},"left")," and\n",(0,n.kt)("inlineCode",{parentName:"p"},"right")," fields as propositions directly, we specify them as Proposition Templates."),(0,n.kt)("h3",{id:"nottemplate"},"NotTemplate"),(0,n.kt)("p",null,"Once built, a NotTemplate refers to\na ",(0,n.kt)(l.Z,{href:"https://github.com/Topl/protobuf-specs/blob/main/proto/quivr/models/proposition.proto#L91",mdxType:"Link"},"Not proposition"),".\nYou can create an NotTemplate using\nthe ",(0,n.kt)(r.Z,{path:"co/topl/brambl/builders/locks/PropositionTemplate$$NotTemplate.html",mdxType:"ScaladocLink"},(0,n.kt)("code",null,"NotTemplate")),"\ncase class:"),(0,n.kt)("pre",null,(0,n.kt)("code",{parentName:"pre",className:"language-scala"},"case class NotTemplate[F[_]: Monad](\n  innerTemplate: PropositionTemplate[F]\n) extends PropositionTemplate[F]\n")),(0,n.kt)("p",null,"The parameters are similar to a Not proposition. The key difference being that, instead of specifying the ",(0,n.kt)("inlineCode",{parentName:"p"},"proposition"),"\nfield as a proposition directly, we specify it as Proposition Templates."),(0,n.kt)("h3",{id:"thresholdtemplate"},"ThresholdTemplate"),(0,n.kt)("p",null,"Once built, a ThresholdTemplate refers to\na ",(0,n.kt)(l.Z,{href:"https://github.com/Topl/protobuf-specs/blob/main/proto/quivr/models/proposition.proto#L85",mdxType:"Link"},"Threshold proposition"),".\nYou can create an ThresholdTemplate using\nthe ",(0,n.kt)(r.Z,{path:"co/topl/brambl/builders/locks/PropositionTemplate$$ThresholdTemplate.html",mdxType:"ScaladocLink"},(0,n.kt)("code",null,"ThresholdTemplate")),"\ncase class:"),(0,n.kt)("pre",null,(0,n.kt)("code",{parentName:"pre",className:"language-scala"},"case class ThresholdTemplate[F[_]: Monad](\n  innerTemplates: Seq[]PropositionTemplate[F]],\n  threshold: Int\n) extends PropositionTemplate[F]\n")),(0,n.kt)("p",null,"The parameters are similar to a Threshold proposition. The key difference being that, instead of specifying the ",(0,n.kt)("inlineCode",{parentName:"p"},"challenges"),"\nfield as propositions directly, we specify it as Proposition Templates."))}k.isMDXComponent=!0}}]);