#include <catch2/catch_test_macros.hpp>
#include "minimx.h"
#include "filesystem"
#include "iostream"

using namespace minimx;

TEST_CASE("MusicXmlParser", "") {
    REQUIRE(std::filesystem::is_directory(TEST_DATA_DIR));
    std::filesystem::path dataDir(TEST_DATA_DIR);

    pugi::xml_document doc;
    doc.load_file((dataDir / "MusicXmlWithStaffValues.xml").c_str());
    MXScore score(doc);

    SECTION("Movement Title") {
        REQUIRE(score.movementTitle == "Im wunderschönen Monat Mai");
    }

    SECTION("Identification") {
        REQUIRE(score.identification.composer == "Robert Schumann");
        REQUIRE(score.identification.rights == "Copyright © 2002 Recordare LLC");
        REQUIRE(score.identification.encoding.software == "Finale 2011 for Windows\nDolet 6.0 for Finale");
        REQUIRE(score.identification.encoding.description == "This is a sample description\nacross multiple lines");
    }

    SECTION("Parts") {
        REQUIRE(score.parts.size() == 2);
        REQUIRE(score.parts[0].id == "P1");
        REQUIRE(score.parts[0].name == "Voice");
    }

    SECTION("Measures") {
        const auto& part = score.parts[0];
        REQUIRE(part.measures.size() == 27);
    }


}