-- https://github.com/aristanetworks/purescript-backend-optimizer#usage
--
-- This package passes all its tests with purescript-backend-optimizer.
--
-- For running:
--
--     spago -x spago-backend-es.dhall test
--
-- which doesn't work because of bugs in spago,
-- https://github.com/aristanetworks/purescript-backend-optimizer/issues/25
-- so run this as a substitute:
--
--     spago -x spago-dev.dhall build --purs-args '--codegen corefn,js'
--     purs-backend-es bundle-app --minify --platform=node --main Test.Main --to index.mjs
--     node index.mjs
--
-- For bench:
--
--     spago -x spago-dev.dhall build --purs-args '--codegen corefn,js'
--     purs-backend-es bundle-app --minify --platform=node --main Bench.Main --to index.mjs
--     node --expose-gc index.mjs > bench2.txt
--

./spago-dev.dhall // { backend = "purs-backend-es build" }

