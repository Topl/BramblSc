"use strict";(self.webpackChunkbramblsc_documentation=self.webpackChunkbramblsc_documentation||[]).push([[472],{3905:(e,t,n)=>{n.d(t,{Zo:()=>d,kt:()=>k});var o=n(7294);function r(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function i(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);t&&(o=o.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,o)}return n}function a(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?i(Object(n),!0).forEach((function(t){r(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):i(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function s(e,t){if(null==e)return{};var n,o,r=function(e,t){if(null==e)return{};var n,o,r={},i=Object.keys(e);for(o=0;o<i.length;o++)n=i[o],t.indexOf(n)>=0||(r[n]=e[n]);return r}(e,t);if(Object.getOwnPropertySymbols){var i=Object.getOwnPropertySymbols(e);for(o=0;o<i.length;o++)n=i[o],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(r[n]=e[n])}return r}var l=o.createContext({}),c=function(e){var t=o.useContext(l),n=t;return e&&(n="function"==typeof e?e(t):a(a({},t),e)),n},d=function(e){var t=c(e.components);return o.createElement(l.Provider,{value:t},e.children)},p="mdxType",u={inlineCode:"code",wrapper:function(e){var t=e.children;return o.createElement(o.Fragment,{},t)}},m=o.forwardRef((function(e,t){var n=e.components,r=e.mdxType,i=e.originalType,l=e.parentName,d=s(e,["components","mdxType","originalType","parentName"]),p=c(n),m=r,k=p["".concat(l,".").concat(m)]||p[m]||u[m]||i;return n?o.createElement(k,a(a({ref:t},d),{},{components:n})):o.createElement(k,a({ref:t},d))}));function k(e,t){var n=arguments,r=t&&t.mdxType;if("string"==typeof e||r){var i=n.length,a=new Array(i);a[0]=m;var s={};for(var l in t)hasOwnProperty.call(t,l)&&(s[l]=t[l]);s.originalType=e,s[p]="string"==typeof e?e:r,a[1]=s;for(var c=2;c<i;c++)a[c]=n[c];return o.createElement.apply(null,a)}return o.createElement.apply(null,n)}m.displayName="MDXCreateElement"},7884:(e,t,n)=>{n.d(t,{Z:()=>s});var o=n(7294),r=n(9960),i=n(6550),a=n(8084);function s(e){let{children:t,path:n}=e;var s=function(){const{pathname:e}=(0,i.TH)(),{versions:t}=(0,a.eZ)("docusaurus-plugin-content-docs");return t.find((t=>{let{path:n}=t;return e.startsWith(n)})).name||"current"}();return o.createElement(r.Z,{to:`/scaladoc/${s}/${n}`,target:"_blank"},t)}},9239:(e,t,n)=>{n.d(t,{ZP:()=>s});var o=n(7462),r=(n(7294),n(3905));const i={toc:[]},a="wrapper";function s(e){let{components:t,...n}=e;return(0,r.kt)(a,(0,o.Z)({},i,n,{components:t,mdxType:"MDXLayout"}),(0,r.kt)("admonition",{type:"caution"},(0,r.kt)("p",{parentName:"admonition"},"By the nature of the blockchain, a UTXO's lock is publicly\nrevealed once it's spent, rendering all other UTXOs encumbered by the same lock insecure and vulnerable to theft. To\nprevent this, we strongly recommend that all other UTXOs encumbered by the same lock are moved to a new lock. Our functions\nare designed to make this easy, however, it is up to the user to ensure that this is done.")))}s.isMDXComponent=!0},6347:(e,t,n)=>{n.d(t,{ZP:()=>s});var o=n(7462),r=(n(7294),n(3905));const i={toc:[]},a="wrapper";function s(e){let{components:t,...n}=e;return(0,r.kt)(a,(0,o.Z)({},i,n,{components:t,mdxType:"MDXLayout"}),(0,r.kt)("span",null,"On success, this function returns an unproven ",(0,r.kt)("code",null,"IoTransaction")," which can be proven and then broadcasted to the network. Once the transaction is processed and accepted on the chain, the ",n.tokenType," tokens can be used in subsequent transactions. On failure, this function returns a ",(0,r.kt)("code",null,"BuilderError")," which contains information on why it failed."))}s.isMDXComponent=!0},9758:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>p,contentTitle:()=>c,default:()=>h,frontMatter:()=>l,metadata:()=>d,toc:()=>u});var o=n(7462),r=(n(7294),n(3905)),i=n(9239),a=n(6347),s=n(7884);const l={sidebar_position:1,title:"Mint Tokens",description:"Build a transaction to mint group, series, and asset tokens."},c="Build a Minting Transaction",d={unversionedId:"reference/transactions/minting",id:"reference/transactions/minting",title:"Mint Tokens",description:"Build a transaction to mint group, series, and asset tokens.",source:"@site/docs/reference/transactions/minting.mdx",sourceDirName:"reference/transactions",slug:"/reference/transactions/minting",permalink:"/BramblSc/docs/current/reference/transactions/minting",draft:!1,tags:[],version:"current",sidebarPosition:1,frontMatter:{sidebar_position:1,title:"Mint Tokens",description:"Build a transaction to mint group, series, and asset tokens."},sidebar:"referenceSidebar",previous:{title:"Build a Transaction",permalink:"/BramblSc/docs/current/reference/transactions/"},next:{title:"Transfer Tokens",permalink:"/BramblSc/docs/current/reference/transactions/transfer"}},p={},u=[{value:"Mint Group Constructor Tokens",id:"mint-group-constructor-tokens",level:2},{value:"Example",id:"example",level:3},{value:"Mint Series Constructor Tokens",id:"mint-series-constructor-tokens",level:2},{value:"Example",id:"example-1",level:3},{value:"Mint Asset Tokens",id:"mint-asset-tokens",level:2},{value:"Example",id:"example-2",level:3}],m={toc:u},k="wrapper";function h(e){let{components:t,...n}=e;return(0,r.kt)(k,(0,o.Z)({},m,n,{components:t,mdxType:"MDXLayout"}),(0,r.kt)("h1",{id:"build-a-minting-transaction"},"Build a Minting Transaction"),(0,r.kt)("p",null,"The first step to minting new tokens on the Blockchain is to build an unproven minting transaction. There are three\ntypes of tokens you can mint; group constructor tokens, series constructor tokens, and asset tokens. The transaction\nbuilder offers a way to build each."),(0,r.kt)(i.ZP,{mdxType:"RevealedLock"}),(0,r.kt)("h2",{id:"mint-group-constructor-tokens"},"Mint Group Constructor Tokens"),(0,r.kt)("p",null,"You can create a transaction to mint new group constructor tokens using\nthe ",(0,r.kt)(s.Z,{path:"co/topl/brambl/builders/TransactionBuilderApi.html#buildGroupMintingTransaction(Seq[Txo],Predicate,GroupPolicy,Long,LockAddress,LockAddress,Long):F[Either[BuilderError,IoTransaction]]",mdxType:"ScaladocLink"},(0,r.kt)("code",null,"buildGroupMintingTransaction")),"\nfunction of a Transaction Builder API instance."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},"def buildGroupMintingTransaction(\n  txos:              Seq[Txo],\n  lockPredicateFrom: Lock.Predicate,\n  groupPolicy:       GroupPolicy,\n  quantityToMint:    Long,\n  mintedAddress:     LockAddress,\n  changeAddress:     LockAddress,\n  fee:               Long\n): F[Either[BuilderError, IoTransaction]]\n")),(0,r.kt)("p",null,'This function builds a transaction to mint group constructor tokens for a given Group Policy. These minted group\nconstructor tokens must be "registered" to an existing LVL UTXO. This registration UTXO is specified in the Group Policy and\nmust be included in the transaction inputs.'),(0,r.kt)("p",null,"The parameters are as follows:"),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("inlineCode",{parentName:"li"},"txos")," - A sequence of TXOs to be the inputs of the created transaction. The registration UTXO (as specified in the\n",(0,r.kt)("inlineCode",{parentName:"li"},"groupPolicy"),") must be included in this sequence. All TXOs must be encumbered by the same lock predicate, given by\n",(0,r.kt)("inlineCode",{parentName:"li"},"lockPredicateFrom"),". You can obtain these TXOs from the ",(0,r.kt)("a",{parentName:"li",href:"../rpc#querying-utxos"},"RPC queries"),"."),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("inlineCode",{parentName:"li"},"lockPredicateFrom")," - The Predicate Lock that encumbers all the TXOs in ",(0,r.kt)("inlineCode",{parentName:"li"},"txos"),"."),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("inlineCode",{parentName:"li"},"groupPolicy")," - The Group Policy for which to mint new group constructor tokens."),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("inlineCode",{parentName:"li"},"quantityToMint")," - The quantity of group constructor tokens to mint."),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("inlineCode",{parentName:"li"},"mintedAddress")," - A new LockAddress for which the minted group constructor tokens will be encumbered. You can create a\nnew Lock and LockAddress ",(0,r.kt)("a",{parentName:"li",href:"../locks"},"with the SDK"),"."),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("inlineCode",{parentName:"li"},"changeAddress")," - A new LockAddress for which all the change UTXOs from ",(0,r.kt)("inlineCode",{parentName:"li"},"txos")," will be encumbered. You can create a new\nLock and LockAddress ",(0,r.kt)("a",{parentName:"li",href:"../locks"},"with the SDK"),"."),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("inlineCode",{parentName:"li"},"fee")," - The transaction fee. The ",(0,r.kt)("inlineCode",{parentName:"li"},"txos")," must contain enough LVLs to satisfy this fee.")),(0,r.kt)(a.ZP,{tokenType:"minted group constructor",mdxType:"TxReturn"}),(0,r.kt)("h3",{id:"example"},"Example"),(0,r.kt)("p",null,"The following example shows how to build a transaction to mint 1 group constructor token for a given Group Policy."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},'import cats.effect.IO\nimport cats.effect.unsafe.implicits.global\nimport co.topl.brambl.builders.TransactionBuilderApi\nimport co.topl.brambl.constants.NetworkConstants.{MAIN_LEDGER_ID, PRIVATE_NETWORK_ID}\nimport co.topl.brambl.builders.locks.LockTemplate\nimport co.topl.brambl.builders.locks.PropositionTemplate.HeightTemplate\nimport co.topl.brambl.builders.locks.LockTemplate.PredicateTemplate\nimport co.topl.brambl.codecs.AddressCodecs.decodeAddress\nimport co.topl.brambl.syntax.{LvlType, valueToTypeIdentifierSyntaxOps}\nimport co.topl.brambl.dataApi.{GenusQueryAlgebra, RpcChannelResource}\nimport co.topl.brambl.models.Event.GroupPolicy\n\n// Mock address. Replace with recipient address.\nval toAddr = decodeAddress("ptetP7jshHTuV9bmPmtVLm6PtUzBMZ8iYRvAxvbGTJ5VgiEPHqCCnZ8MLLdi").toOption.get\n\n// Replace with the address and port of your node\'s gRPC endpoint\nval channelResource = RpcChannelResource.channelResource[IO]("localhost", 9084, secureConnection = false)\n\nval transactionBuilderApi = TransactionBuilderApi.make[IO](PRIVATE_NETWORK_ID, MAIN_LEDGER_ID)\nval predicateTemplate: LockTemplate[IO] = PredicateTemplate[IO](Seq(HeightTemplate("header", 1, Long.MaxValue)), 1)\n\n// Transaction building starts here:\nval tx = for {\n  fromLock <- predicateTemplate.build(List.empty).map(_.toOption.get)\n  fromAddr <- transactionBuilderApi.lockAddress(fromLock)\n  fromTxos <- GenusQueryAlgebra.make[IO](channelResource).queryUtxo(fromAddr)\n  policy = GroupPolicy("Group Policy Label", fromTxos.filter(_.transactionOutput.value.value.typeIdentifier == LvlType).head.outputAddress)\n  // highlight-next-line\n  res <- transactionBuilderApi.buildGroupMintingTransaction(fromTxos, fromLock.getPredicate, policy, 1L, toAddr, toAddr, 1L)\n} yield res\n\ntx.unsafeRunSync()\n')),(0,r.kt)("h2",{id:"mint-series-constructor-tokens"},"Mint Series Constructor Tokens"),(0,r.kt)("p",null,"You can create a transaction to mint new series constructor tokens using\nthe ",(0,r.kt)(s.Z,{path:"co/topl/brambl/builders/TransactionBuilderApi.html#buildSeriesMintingTransaction(Seq[Txo],Predicate,SeriesPolicy,Long,LockAddress,LockAddress,Long):F[Either[BuilderError,IoTransaction]]",mdxType:"ScaladocLink"},(0,r.kt)("code",null,"buildSeriesMintingTransaction")),"\nfunction of a Transaction Builder API instance."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},"def buildSeriesMintingTransaction(\n  txos:              Seq[Txo],\n  lockPredicateFrom: Lock.Predicate,\n  seriesPolicy:      SeriesPolicy,\n  quantityToMint:    Long,\n  mintedAddress:     LockAddress,\n  changeAddress:     LockAddress,\n  fee:               Long\n): F[Either[BuilderError, IoTransaction]]\n")),(0,r.kt)("p",null,'This function builds a transaction to mint series constructor tokens for a given Series Policy. These minted series\nconstructor tokens must be "registered" to an existing LVL UTXO. This registration UTXO is specified in the Series Policy\nand must be included in the transaction inputs.'),(0,r.kt)("p",null,"The parameters are as follows:"),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("inlineCode",{parentName:"li"},"txos")," - A sequence of TXOs to be the inputs of the created transaction. The registration UTXO (as specified in the\n",(0,r.kt)("inlineCode",{parentName:"li"},"seriesPolicy"),") must be included in this sequence. All TXOs must be encumbered by the same lock predicate, given by\n",(0,r.kt)("inlineCode",{parentName:"li"},"lockPredicateFrom"),". You can obtain these TXOs from the ",(0,r.kt)("a",{parentName:"li",href:"../rpc#querying-utxos"},"RPC queries"),"."),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("inlineCode",{parentName:"li"},"lockPredicateFrom")," - The Predicate Lock that encumbers all the TXOs in ",(0,r.kt)("inlineCode",{parentName:"li"},"txos"),"."),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("inlineCode",{parentName:"li"},"seriesPolicy")," - The Series Policy for which to mint new series constructor tokens."),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("inlineCode",{parentName:"li"},"quantityToMint")," - The quantity of series constructor tokens to mint."),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("inlineCode",{parentName:"li"},"mintedAddress")," - A new LockAddress for which the minted series constructor tokens will be encumbered. You can create a\nnew Lock and LockAddress ",(0,r.kt)("a",{parentName:"li",href:"../locks"},"with the SDK"),"."),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("inlineCode",{parentName:"li"},"changeAddress")," - A new LockAddress for which all the change UTXOs from ",(0,r.kt)("inlineCode",{parentName:"li"},"txos")," will be encumbered. You can create a new\nLock and LockAddress ",(0,r.kt)("a",{parentName:"li",href:"../locks"},"with the SDK"),"."),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("inlineCode",{parentName:"li"},"fee")," - The transaction fee. The ",(0,r.kt)("inlineCode",{parentName:"li"},"txos")," must contain enough LVLs to satisfy this fee.")),(0,r.kt)(a.ZP,{tokenType:"minted series constructor",mdxType:"TxReturn"}),(0,r.kt)("h3",{id:"example-1"},"Example"),(0,r.kt)("p",null,"The following example shows how to build a transaction to mint 1 series constructor token for a given Series Policy."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},'import cats.effect.IO\nimport cats.effect.unsafe.implicits.global\nimport co.topl.brambl.builders.TransactionBuilderApi\nimport co.topl.brambl.constants.NetworkConstants.{MAIN_LEDGER_ID, PRIVATE_NETWORK_ID}\nimport co.topl.brambl.builders.locks.LockTemplate\nimport co.topl.brambl.builders.locks.PropositionTemplate.HeightTemplate\nimport co.topl.brambl.builders.locks.LockTemplate.PredicateTemplate\nimport co.topl.brambl.codecs.AddressCodecs.decodeAddress\nimport co.topl.brambl.syntax.{LvlType, valueToTypeIdentifierSyntaxOps}\nimport co.topl.brambl.dataApi.{GenusQueryAlgebra, RpcChannelResource}\nimport co.topl.brambl.models.Event.SeriesPolicy\n\n// Mock address. Replace with recipient address.\nval toAddr = decodeAddress("ptetP7jshHTuV9bmPmtVLm6PtUzBMZ8iYRvAxvbGTJ5VgiEPHqCCnZ8MLLdi").toOption.get\n\n// Replace with the address and port of your node\'s gRPC endpoint\nval channelResource = RpcChannelResource.channelResource[IO]("localhost", 9084, secureConnection = false)\n\nval transactionBuilderApi = TransactionBuilderApi.make[IO](PRIVATE_NETWORK_ID, MAIN_LEDGER_ID)\nval predicateTemplate: LockTemplate[IO] = PredicateTemplate[IO](Seq(HeightTemplate("header", 1, Long.MaxValue)), 1)\n\n// Transaction building starts here:\nval tx = for {\n  fromLock <- predicateTemplate.build(List.empty).map(_.toOption.get)\n  fromAddr <- transactionBuilderApi.lockAddress(fromLock)\n  fromTxos <- GenusQueryAlgebra.make[IO](channelResource).queryUtxo(fromAddr)\n  policy = SeriesPolicy("Series Policy Label", registrationUtxo= fromTxos.filter(_.transactionOutput.value.value.typeIdentifier == LvlType).head.outputAddress)\n  // highlight-next-line\n  res <- transactionBuilderApi.buildSeriesMintingTransaction(fromTxos, fromLock.getPredicate, policy, 1L, toAddr, toAddr, 1L)\n} yield res\n\ntx.unsafeRunSync()\n')),(0,r.kt)("h2",{id:"mint-asset-tokens"},"Mint Asset Tokens"),(0,r.kt)("p",null,"You can create a transaction to mint new asset tokens using\nthe ",(0,r.kt)(s.Z,{path:"co/topl/brambl/builders/TransactionBuilderApi.html#buildAssetMintingTransaction(AssetMintingStatement,Seq[Txo],Map[LockAddress,Predicate],Long,LockAddress,LockAddress,Option[Struct],Option[ByteString]):F[Either[BuilderError,IoTransaction]]",mdxType:"ScaladocLink"},(0,r.kt)("code",null,"buildAssetMintingTransaction")),"\nfunction of a Transaction Builder API instance."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},"def buildAssetMintingTransaction(\n  mintingStatement:       AssetMintingStatement,\n  txos:                   Seq[Txo],\n  locks:                  Map[LockAddress, Lock.Predicate],\n  fee:                    Long,\n  mintedAssetLockAddress: LockAddress,\n  changeAddress:          LockAddress,\n  ephemeralMetadata:      Option[Struct] = None,\n  commitment:             Option[ByteString] = None\n): F[Either[BuilderError, IoTransaction]]\n")),(0,r.kt)("p",null,"This function builds a transaction to mint asset tokens for a given Asset Minting Statement. These minted asset\ntokens must be created from existing group and series constructor tokens. These registration constructor tokens are\nspecified in the Asset Minting Statement and must be included in the transaction inputs."),(0,r.kt)("p",null,"The parameters are as follows:"),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("inlineCode",{parentName:"li"},"mintingStatement")," - The minting statement that specifies the asset to mint."),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("inlineCode",{parentName:"li"},"txos")," - A sequence of TXOs to be the inputs of the created transaction. Both the group and series constructor tokens\nthat are specified in the ",(0,r.kt)("inlineCode",{parentName:"li"},"mintingStatement")," must be included in this sequence. All TXOs must be encumbered by a lock\nfrom ",(0,r.kt)("inlineCode",{parentName:"li"},"locks"),". You can obtain these TXOs from the ",(0,r.kt)("a",{parentName:"li",href:"../rpc#querying-utxos"},"RPC queries"),"."),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("inlineCode",{parentName:"li"},"locks")," - A mapping of LockAddress to their Predicate Lock. Each entry must represent a lock that encumbers some TXOs\nin ",(0,r.kt)("inlineCode",{parentName:"li"},"txos"),"."),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("inlineCode",{parentName:"li"},"fee")," - The transaction fee. The ",(0,r.kt)("inlineCode",{parentName:"li"},"txos")," must contain enough LVLs to satisfy this fee."),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("inlineCode",{parentName:"li"},"mintedAssetLockAddress")," - A new LockAddress for which the minted asset tokens will be encumbered. You can create a\nnew Lock and LockAddress ",(0,r.kt)("a",{parentName:"li",href:"../locks"},"with the SDK"),"."),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("inlineCode",{parentName:"li"},"changeAddress")," - A new LockAddress for which all the change UTXOs from ",(0,r.kt)("inlineCode",{parentName:"li"},"txos")," will be encumbered. You can create a new\nLock and LockAddress ",(0,r.kt)("a",{parentName:"li",href:"../locks"},"with the SDK"),"."),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("inlineCode",{parentName:"li"},"ephemeralMetadata")," - Optional ephemeral metadata to include in the minted asset tokens."),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("inlineCode",{parentName:"li"},"commitment")," - Optional commitment to include in the minted asset tokens.")),(0,r.kt)("admonition",{type:"note"},(0,r.kt)("p",{parentName:"admonition"},"If the ",(0,r.kt)("inlineCode",{parentName:"p"},"tokenSupply")," field is present in the series constructor token used for registration, then the quantity of asset\ntokens to mint (that is defined in the ",(0,r.kt)("inlineCode",{parentName:"p"},"mintingStatement"),") has to be a multiple of this field. In this case, minting\neach multiple of ",(0,r.kt)("inlineCode",{parentName:"p"},"tokenSupply")," quantity of assets will burn a single series constructor token.")),(0,r.kt)(a.ZP,{tokenType:"minted asset",mdxType:"TxReturn"}),(0,r.kt)("h3",{id:"example-2"},"Example"),(0,r.kt)("p",null,"The following example shows how to build a transaction to mint 1 asset token for a given Group Policy and a given Series Policy.\nIn this example, we first build, prove, and broadcast a transaction to register a group and a transaction to register a series.\nBoth of which are needed before we can mint asset tokens. Notice that we wait 15 seconds before re-querying Genus for spendable UTXOs.\nThis is because it takes some time for the transactions to be included in Genus. The delay on the Bifrost node, however,\nis much shorter thus we were able to only wait 1 second in the example."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},'import cats.effect.IO\nimport cats.effect.unsafe.implicits.global\nimport co.topl.brambl.builders.TransactionBuilderApi\nimport co.topl.brambl.constants.NetworkConstants.{MAIN_LEDGER_ID, PRIVATE_NETWORK_ID}\nimport co.topl.brambl.builders.locks.LockTemplate\nimport co.topl.brambl.builders.locks.PropositionTemplate.HeightTemplate\nimport co.topl.brambl.builders.locks.LockTemplate.PredicateTemplate\nimport co.topl.brambl.syntax.{GroupType, LvlType, SeriesType, cryptoToPbKeyPair, groupPolicyAsGroupPolicySyntaxOps, longAsInt128, seriesPolicyAsSeriesPolicySyntaxOps, valueToTypeIdentifierSyntaxOps}\nimport co.topl.brambl.dataApi.{BifrostQueryAlgebra, GenusQueryAlgebra, RpcChannelResource}\nimport co.topl.brambl.models.Event.{GroupPolicy, SeriesPolicy}\nimport co.topl.brambl.models.box.AssetMintingStatement\nimport co.topl.brambl.servicekit.{WalletKeyApi, WalletStateApi, WalletStateResource}\nimport co.topl.brambl.wallet.{CredentiallerInterpreter, WalletApi}\nimport co.topl.crypto.signing.ExtendedEd25519\n\n// Replace with the address and port of your node\'s gRPC endpoint\nval channelResource = RpcChannelResource.channelResource[IO]("localhost", 9084, secureConnection = false)\nval genusQuery = GenusQueryAlgebra.make[IO](channelResource)\nval bifrostQuery = BifrostQueryAlgebra.make[IO](channelResource)\n\n// Replace with the location of your wallet state file\nval walletConnection = WalletStateResource.walletResource("wallet.db")\n\n// Some mock key pair. Do not use. Replace with your own main key pair.\nval mainKey = (new ExtendedEd25519).deriveKeyPairFromSeed(Array.fill(96)(0: Byte))\n\nval walletApi = WalletApi.make[IO](WalletKeyApi.make())\nval walletStateApi = WalletStateApi.make[IO](walletConnection, walletApi)\nval credentialler = CredentiallerInterpreter.make[IO](walletApi, walletStateApi, mainKey)\nval transactionBuilderApi = TransactionBuilderApi.make[IO](PRIVATE_NETWORK_ID, MAIN_LEDGER_ID)\nval predicateTemplate: LockTemplate[IO] = PredicateTemplate[IO](Seq(HeightTemplate("header", 1, Long.MaxValue)), 1)\n\n// Build, prove, and broadcast a transaction to mint a Group Constructor Token:\nval groupTx = for {\n  fromLock <- predicateTemplate.build(List.empty).map(_.toOption.get)\n  fromAddr <- transactionBuilderApi.lockAddress(fromLock)\n  // Replace with the address you want to send the token and change to. for this example, we\'ll send the token to the input address for simplicity.\n  // In production, you\'ll want to send it to a different address for security.\n  toAddr = fromAddr\n  fromTxos <- genusQuery.queryUtxo(fromAddr)\n  policy = GroupPolicy("Group Policy Label", fromTxos.filter(_.transactionOutput.value.value.typeIdentifier == LvlType).head.outputAddress)\n  unprovenTx <- transactionBuilderApi.buildGroupMintingTransaction(fromTxos, fromLock.getPredicate, policy, 1L, toAddr, toAddr, 1L)\n  provenTx <- credentialler.prove(unprovenTx.toOption.get)\n  broadcast <- bifrostQuery.broadcastTransaction(provenTx)\n} yield (broadcast, policy.computeId)\n\nval groupId = groupTx.unsafeRunSync()._2\n\n// Allow some time to pass before querying the transaction\nThread.sleep(15000)\n\n// Build, prove, and broadcast a transaction to mint a Series Constructor Token:\nval seriesTx = for {\n  fromLock <- predicateTemplate.build(List.empty).map(_.toOption.get)\n  fromAddr <- transactionBuilderApi.lockAddress(fromLock)\n  // Replace with the address you want to send the token and change to. for this example, we\'ll send the token to the input address for simplicity.\n  // In production, you\'ll want to send it to a different address for security.\n  toAddr = fromAddr\n  fromTxos <- genusQuery.queryUtxo(fromAddr)\n  policy = SeriesPolicy("Series Policy Label", registrationUtxo= fromTxos.filter(_.transactionOutput.value.value.typeIdentifier == LvlType).head.outputAddress)\n  unprovenTx <- transactionBuilderApi.buildSeriesMintingTransaction(fromTxos, fromLock.getPredicate, policy, 1L, toAddr, toAddr, 1L)\n  provenTx <- credentialler.prove(unprovenTx.toOption.get)\n  broadcast <- bifrostQuery.broadcastTransaction(provenTx)\n} yield (broadcast, policy.computeId)\n\nval seriedId = seriesTx.unsafeRunSync()._2\n\n// Allow some time to pass before querying the transaction\nThread.sleep(15000)\n\n// Build, prove, and broadcast a transaction to mint a Asset Token begins here:\nval assetTx = for {\n  fromLock <- predicateTemplate.build(List.empty).map(_.toOption.get)\n  fromAddr <- transactionBuilderApi.lockAddress(fromLock)\n  // Replace with the address you want to send the token and change to. for this example, we\'ll send the token to the input address for simplicity.\n  // In production, you\'ll want to send it to a different address for security.\n  toAddr = fromAddr\n  fromTxos <- genusQuery.queryUtxo(fromAddr)\n  mintingStatement = AssetMintingStatement(\n    fromTxos.filter(_.transactionOutput.value.value.typeIdentifier == GroupType(groupId)).head.outputAddress,\n    fromTxos.filter(_.transactionOutput.value.value.typeIdentifier == SeriesType(seriedId)).head.outputAddress,\n    1\n  )\n  // highlight-start\n  unprovenTx <- transactionBuilderApi.buildAssetMintingTransaction(\n    mintingStatement,\n    fromTxos,\n    Map(fromAddr -> fromLock.getPredicate),\n    1L,\n    toAddr,\n    toAddr\n  )\n  // highlight-end\n  provenTx <- credentialler.prove(unprovenTx.toOption.get)\n  broadcast <- bifrostQuery.broadcastTransaction(provenTx)\n} yield broadcast\n\nval txId = assetTx.unsafeRunSync()\n\n// Allow some time to pass before querying the transaction\nThread.sleep(1000)\n\nval postedTransaction = bifrostQuery.fetchTransaction(txId).unsafeRunSync()\n')))}h.isMDXComponent=!0}}]);