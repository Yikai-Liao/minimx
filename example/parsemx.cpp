//
// Created by Yikai Liao on 24-12-20.
//
#define PUGIXML_COMPACT

#include "minimx.h"
#include "iostream"
#include "filesystem"
#include <format>
#include <nanobench.h>

using namespace minimx;

std::filesystem::path dataDir(TEST_DATA_DIR);

auto load_score(const std::filesystem::path& p) {
    pugi::xml_document doc;
    doc.load_file(p.c_str());
    MXScore score(doc);
    return score;
}

auto load_doc(const std::filesystem::path& p) {
    pugi::xml_document doc;
    doc.load_file(p.c_str());
    return doc;
}

int main() {
    std::cout << dataDir << std::endl;
    const auto p = dataDir / "MusicXmlWithStaffValues.xml";

    pugi::xml_document doc;
    doc.load_file(p.c_str());
    // collect all measure nodes using xpath
    const auto measureNodes = doc.select_nodes("//part/measure");
    const auto noteNodes    = doc.select_nodes("//part/measure/note");
    // show measure number
    std::cout << "Number of measures: " << measureNodes.size() << std::endl;
    std::cout << "Number of notes: " << noteNodes.size() << std::endl;
    // for (auto i=0; i<1000; i++) {
    //     MXScore score(doc);
    //     ankerl::nanobench::doNotOptimizeAway(score);
    // }
    auto partwise = doc.child("score-partwise");

    ankerl::nanobench::Bench()
        .minEpochIterations(500)
        .run(
            "Parse Whole MusicXML",
            [&]() {
                MXScore score(doc);
                ankerl::nanobench::doNotOptimizeAway(score);
            }
        )
        .run("Parse Document", [&]() { ankerl::nanobench::doNotOptimizeAway(load_doc(p)); })
        .run(
            "Xpath Select Part",
            [&]() { ankerl::nanobench::doNotOptimizeAway(partwise.children("part")); }
        )
        .run(
            "Xpath Select Measure",
            [&]() { ankerl::nanobench::doNotOptimizeAway(doc.select_nodes("//part/measure")); }
        )
        .run(
            "Xpath Select Note",
            [&]() { ankerl::nanobench::doNotOptimizeAway(doc.select_nodes("//part/measure/note")); }
        )
        .run(
            "Parse Measure",
            [&]() {
                for (const auto& measureNode : measureNodes) {
                    Measure measure(measureNode.node());
                }
            }
        )
        .run(
            "Parse Note",
            [&]() {
                for (const auto& noteNode : noteNodes) {
                    MeasureElement noteElement(noteNode.node());
                }
            }
        )
        .run(
            "Dispatch Element Type",
            [&]() {
                for (const auto& noteNode : noteNodes) {
                    build_measure_element_type(noteNode.node().name());
                }
            }
        )
        .run(
            "Build Tie",
            [&]() {
                for (const auto& noteNode : noteNodes) { build_tie(noteNode.node()); }
            }
        )
        .run(
            "Parse Bool Member",
            [&]() {
                for (const auto& noteNode : noteNodes) {
                    bool isRest  = noteNode.node().child("rest");
                    bool isGrace = noteNode.node().child("grace");
                    bool isChord = noteNode.node().child("chord");
                    ankerl::nanobench::doNotOptimizeAway(isRest);
                    ankerl::nanobench::doNotOptimizeAway(isGrace);
                    ankerl::nanobench::doNotOptimizeAway(isChord);
                }
            }
        )
        .run(
            "Parse Int Member",
            [&]() {
                for (const auto& noteNode : noteNodes) {
                    int duration = noteNode.node().child("duration").text().as_int();
                    int voice    = noteNode.node().child("voice").text().as_int();
                    int staff    = noteNode.node().child("staff").text().as_int();
                    ankerl::nanobench::doNotOptimizeAway(duration);
                    ankerl::nanobench::doNotOptimizeAway(voice);
                    ankerl::nanobench::doNotOptimizeAway(staff);
                }
            }
        )
        .run(
            "Parse Pitch",
            [&]() {
                for (const auto& noteNode : noteNodes) {
                    bool isRest = noteNode.node().child("rest");
                    if (!isRest) { ankerl::nanobench::doNotOptimizeAway(Pitch(noteNode.node())); }
                }
            }
        )
        .run("Parse Lyric", [&]() {
            for (const auto& noteNode : noteNodes) { Lyric(noteNode.node()); }
        });

    return 0;
}