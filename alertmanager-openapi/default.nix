{ mkDerivation, aeson, base, base64-bytestring, bytestring
      , case-insensitive, containers, deepseq, exceptions, hspec
      , http-api-data, http-client, http-client-tls, http-media
      , http-types, iso8601-time, lib, microlens, monad-logger, mtl
      , network, QuickCheck, random, safe-exceptions, semigroups, text
      , time, transformers, unordered-containers, vector, cabal-install
      }:
      mkDerivation {
        pname = "alertmanager-openapi";
        version = "0.0.1.0";
        src = ./.;
        libraryToolDepends = [cabal-install];
        libraryHaskellDepends = [
          aeson base base64-bytestring bytestring case-insensitive containers
          deepseq exceptions http-api-data http-client http-client-tls
          http-media http-types iso8601-time microlens monad-logger mtl
          network random safe-exceptions text time transformers
          unordered-containers vector
        ];
        testHaskellDepends = [
          aeson base bytestring containers hspec iso8601-time mtl QuickCheck
          semigroups text time transformers unordered-containers vector
        ];
        homepage = "https://openapi-generator.tech";
        description = "Auto-generated alertmanager-openapi API Client";
        license = lib.licenses.bsd3;
      }
