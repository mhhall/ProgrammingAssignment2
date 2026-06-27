import SwiftUI
import SwiftData

// MARK: - Picker option lists

private let csStyles = [
    "Standard", "Vivid", "Neutral", "Clear", "Deep", "Light",
    "Portrait", "Landscape", "Sunset", "Night Scene", "Autumn Leaves",
    "Black & White", "Sepia"
]
private let ppGammas = [
    "Movie", "Still", "Cine1", "Cine2", "Cine3", "Cine4",
    "ITU709", "ITU709(800%)", "S-Log2", "S-Log3",
    "HLG", "HLG1", "HLG2", "HLG3"
]
private let ppColorModes = [
    "Movie", "Still", "Cinema", "Pro", "ITU709 Matrix", "Black & White",
    "S-Gamut", "S-Gamut3.Cine", "S-Gamut3", "BT.2020", "BT.709"
]
private let ppSlots = (1...10).map { "PP\($0)" }
private let bgRanges = ["Low", "Mid", "High"]
private let kneeSensitivities = ["Low", "Mid", "High"]
private let detailModes = ["Auto", "Manual"]
private let bwBalances = ["Type1", "Type2", "Type3", "Type4", "Type5"]

// MARK: - Add Recipe View

struct AddRecipeView: View {
    @Environment(\.modelContext) private var context
    @Environment(\.dismiss) private var dismiss

    // Basic
    @State private var name = ""
    @State private var description = ""
    @State private var category: RecipeCategory = .street
    @State private var tagsText = ""
    @State private var source = "Custom"
    @State private var settingType: SettingType = .creativeStyle

    // Creative Style
    @State private var csStyle = "Standard"
    @State private var csContrast = 0
    @State private var csSaturation = 0
    @State private var csSharpness = 0
    @State private var csWhiteBalance = "Auto"
    @State private var csWBShift = ""
    @State private var csISO = "Auto"
    @State private var csExposureComp = "0"

    // Picture Profile — core
    @State private var ppSlot = "PP1"
    @State private var ppGamma = "Still"
    @State private var ppBlackLevel = 0
    @State private var ppColorMode = "Still"
    @State private var ppSaturation = 0
    @State private var ppColorPhase = 0
    @State private var ppDetailLevel = 0
    @State private var ppWhiteBalance = "Auto"
    @State private var ppWBShift = ""
    @State private var ppISO = "Auto"
    @State private var ppExposureComp = "0"

    // Picture Profile — Black Gamma
    @State private var ppHasBlackGamma = false
    @State private var ppBGRange = "Mid"
    @State private var ppBGLevel = 0

    // Picture Profile — Knee
    @State private var ppHasKnee = false
    @State private var ppKneeMode = "Auto"
    @State private var ppKneeSensitivity = "Mid"
    @State private var ppKneePoint = "75%"
    @State private var ppKneeSlope = 0

    // Picture Profile — Color Depth
    @State private var ppHasColorDepth = false
    @State private var ppDepthR = 0, ppDepthG = 0, ppDepthB = 0
    @State private var ppDepthC = 0, ppDepthM = 0, ppDepthY = 0

    // Picture Profile — Detail advanced
    @State private var ppHasDetailAdv = false
    @State private var ppDetailMode = "Auto"
    @State private var ppDetailVH = 0
    @State private var ppDetailBW = "Type3"
    @State private var ppDetailLimit = 4
    @State private var ppDetailCrispening = 0
    @State private var ppDetailHLDetail = 0

    private var canSave: Bool { !name.trimmingCharacters(in: .whitespaces).isEmpty }

    var body: some View {
        NavigationStack {
            Form {
                basicSection
                settingTypeSection
                if settingType == .creativeStyle {
                    csSection
                } else {
                    ppCoreSection
                    ppBlackGammaSection
                    ppKneeSection
                    ppColorDepthSection
                    ppDetailAdvSection
                    ppWBSection
                }
            }
            .navigationTitle("New Recipe")
            .navigationBarTitleDisplayMode(.inline)
            .toolbar {
                ToolbarItem(placement: .topBarLeading) {
                    Button("Cancel") { dismiss() }
                }
                ToolbarItem(placement: .topBarTrailing) {
                    Button("Save") { save() }
                        .fontWeight(.semibold)
                        .disabled(!canSave)
                }
            }
        }
    }

    // MARK: - Sections

    private var basicSection: some View {
        Section("Basic Info") {
            TextField("Recipe Name", text: $name)
            TextField("Description", text: $description, axis: .vertical)
                .lineLimit(3...6)
            Picker("Category", selection: $category) {
                ForEach(RecipeCategory.allCases) { cat in
                    Text(cat.rawValue).tag(cat)
                }
            }
            TextField("Tags (comma-separated)", text: $tagsText)
                .foregroundStyle(.secondary)
            TextField("Source", text: $source)
                .foregroundStyle(.secondary)
        }
    }

    private var settingTypeSection: some View {
        Section("Setting Type") {
            Picker("Type", selection: $settingType) {
                ForEach(SettingType.allCases) { t in
                    Text(t.rawValue).tag(t)
                }
            }
            .pickerStyle(.segmented)
            .listRowBackground(Color.clear)
            .listRowInsets(EdgeInsets(top: 8, leading: 0, bottom: 8, trailing: 0))
        }
    }

    private var csSection: some View {
        Section("Creative Style Settings") {
            Picker("Base Style", selection: $csStyle) {
                ForEach(csStyles, id: \.self) { Text($0) }
            }
            SignedStepper("Contrast", value: $csContrast, range: -3...3)
            SignedStepper("Saturation", value: $csSaturation, range: -3...3)
            SignedStepper("Sharpness", value: $csSharpness, range: -3...3)
            LabeledTextField("White Balance", text: $csWhiteBalance, placeholder: "Auto")
            LabeledTextField("WB Color Filter", text: $csWBShift, placeholder: "e.g. B1, G0")
            LabeledTextField("ISO", text: $csISO, placeholder: "Auto")
            LabeledTextField("Exposure Comp", text: $csExposureComp, placeholder: "0")
        }
    }

    private var ppCoreSection: some View {
        Section("Picture Profile Settings") {
            Picker("Profile Slot", selection: $ppSlot) {
                ForEach(ppSlots, id: \.self) { Text($0) }
            }
            SignedStepper("Black Level", value: $ppBlackLevel, range: -15...15)
            Picker("Gamma", selection: $ppGamma) {
                ForEach(ppGammas, id: \.self) { Text($0) }
            }
            Picker("Color Mode", selection: $ppColorMode) {
                ForEach(ppColorModes, id: \.self) { Text($0) }
            }
            SignedStepper("Saturation", value: $ppSaturation, range: -32...32)
            SignedStepper("Color Phase", value: $ppColorPhase, range: -7...7)
            SignedStepper("Detail Level", value: $ppDetailLevel, range: -7...7)
        }
    }

    private var ppBlackGammaSection: some View {
        Section {
            Toggle("Set Black Gamma", isOn: $ppHasBlackGamma)
            if ppHasBlackGamma {
                Picker("BG Range", selection: $ppBGRange) {
                    ForEach(bgRanges, id: \.self) { Text($0) }
                }
                SignedStepper("BG Level", value: $ppBGLevel, range: -7...7)
            }
        } header: {
            Text("Black Gamma")
        }
    }

    private var ppKneeSection: some View {
        Section {
            Toggle("Set Knee", isOn: $ppHasKnee)
            if ppHasKnee {
                Picker("Knee Mode", selection: $ppKneeMode) {
                    Text("Auto").tag("Auto")
                    Text("Manual").tag("Manual")
                }
                .pickerStyle(.segmented)
                if ppKneeMode == "Auto" {
                    Picker("Sensitivity", selection: $ppKneeSensitivity) {
                        ForEach(kneeSensitivities, id: \.self) { Text($0) }
                    }
                } else {
                    LabeledTextField("Knee Point", text: $ppKneePoint, placeholder: "e.g. 75%")
                    SignedStepper("Knee Slope", value: $ppKneeSlope, range: -5...5)
                }
            }
        } header: {
            Text("Knee")
        }
    }

    private var ppColorDepthSection: some View {
        Section {
            Toggle("Set Color Depth", isOn: $ppHasColorDepth)
            if ppHasColorDepth {
                SignedStepper("R", value: $ppDepthR, range: -7...7)
                SignedStepper("G", value: $ppDepthG, range: -7...7)
                SignedStepper("B", value: $ppDepthB, range: -7...7)
                SignedStepper("C", value: $ppDepthC, range: -7...7)
                SignedStepper("M", value: $ppDepthM, range: -7...7)
                SignedStepper("Y", value: $ppDepthY, range: -7...7)
            }
        } header: {
            Text("Color Depth")
        }
    }

    private var ppDetailAdvSection: some View {
        Section {
            Toggle("Advanced Detail Settings", isOn: $ppHasDetailAdv)
            if ppHasDetailAdv {
                Picker("Detail Mode", selection: $ppDetailMode) {
                    ForEach(detailModes, id: \.self) { Text($0) }
                }
                SignedStepper("V/H Balance", value: $ppDetailVH, range: -2...2)
                Picker("B/W Balance", selection: $ppDetailBW) {
                    ForEach(bwBalances, id: \.self) { Text($0) }
                }
                UnsignedStepper("Limit", value: $ppDetailLimit, range: 0...7)
                UnsignedStepper("Crispening", value: $ppDetailCrispening, range: 0...7)
                UnsignedStepper("H-Light Detail", value: $ppDetailHLDetail, range: 0...4)
            }
        } header: {
            Text("Detail Advanced")
        }
    }

    private var ppWBSection: some View {
        Section("White Balance & Exposure") {
            LabeledTextField("White Balance", text: $ppWhiteBalance, placeholder: "Auto")
            LabeledTextField("WB Color Filter", text: $ppWBShift, placeholder: "e.g. A1, G4.5")
            LabeledTextField("ISO", text: $ppISO, placeholder: "Auto")
            LabeledTextField("Exposure Comp", text: $ppExposureComp, placeholder: "0")
        }
    }

    // MARK: - Save

    private func save() {
        let tags = tagsText.split(separator: ",")
            .map { $0.trimmingCharacters(in: .whitespaces) }
            .filter { !$0.isEmpty }

        let recipe = UserRecipe(
            name: name.trimmingCharacters(in: .whitespaces),
            recipeDescription: description,
            categoryRaw: category.rawValue,
            tags: tags,
            source: source.isEmpty ? "Custom" : source,
            settingTypeRaw: settingType.rawValue,
            csStyle: csStyle,
            csContrast: csContrast, csSaturation: csSaturation, csSharpness: csSharpness,
            csWhiteBalance: csWhiteBalance.isEmpty ? "Auto" : csWhiteBalance,
            csWBShift: csWBShift.isEmpty ? nil : csWBShift,
            csISO: csISO.isEmpty ? "Auto" : csISO,
            csExposureComp: csExposureComp.isEmpty ? "0" : csExposureComp,
            ppProfileSlot: ppSlot,
            ppGamma: ppGamma,
            ppBlackLevel: ppBlackLevel,
            ppColorMode: ppColorMode,
            ppSaturation: ppSaturation,
            ppColorPhase: ppColorPhase,
            ppDetailLevel: ppDetailLevel,
            ppWhiteBalance: ppWhiteBalance.isEmpty ? "Auto" : ppWhiteBalance,
            ppWBShift: ppWBShift.isEmpty ? nil : ppWBShift,
            ppISO: ppISO.isEmpty ? "Auto" : ppISO,
            ppExposureComp: ppExposureComp.isEmpty ? "0" : ppExposureComp,
            ppBlackGammaRange: ppHasBlackGamma ? ppBGRange : nil,
            ppBlackGammaLevel: ppHasBlackGamma ? ppBGLevel : nil,
            ppKneeMode: ppHasKnee ? ppKneeMode : nil,
            ppKneeAutoSensitivity: (ppHasKnee && ppKneeMode == "Auto") ? ppKneeSensitivity : nil,
            ppKneeManualPoint: (ppHasKnee && ppKneeMode == "Manual") ? ppKneePoint : nil,
            ppKneeManualSlope: (ppHasKnee && ppKneeMode == "Manual") ? ppKneeSlope : nil,
            ppColorDepthR: ppHasColorDepth ? ppDepthR : nil,
            ppColorDepthG: ppHasColorDepth ? ppDepthG : nil,
            ppColorDepthB: ppHasColorDepth ? ppDepthB : nil,
            ppColorDepthC: ppHasColorDepth ? ppDepthC : nil,
            ppColorDepthM: ppHasColorDepth ? ppDepthM : nil,
            ppColorDepthY: ppHasColorDepth ? ppDepthY : nil,
            ppDetailMode: ppHasDetailAdv ? ppDetailMode : nil,
            ppDetailVHBalance: ppHasDetailAdv ? ppDetailVH : nil,
            ppDetailBWBalance: ppHasDetailAdv ? ppDetailBW : nil,
            ppDetailLimit: ppHasDetailAdv ? ppDetailLimit : nil,
            ppDetailCrispening: ppHasDetailAdv ? ppDetailCrispening : nil,
            ppDetailHighLightDetail: ppHasDetailAdv ? ppDetailHLDetail : nil
        )
        context.insert(recipe)
        dismiss()
    }
}

// MARK: - Helper field views

private struct SignedStepper: View {
    let label: String
    @Binding var value: Int
    let range: ClosedRange<Int>

    var body: some View {
        Stepper(value: $value, in: range) {
            HStack {
                Text(label)
                Spacer()
                Text(value.signedString)
                    .foregroundStyle(.secondary)
                    .monospacedDigit()
            }
        }
    }
}

private struct UnsignedStepper: View {
    let label: String
    @Binding var value: Int
    let range: ClosedRange<Int>

    var body: some View {
        Stepper(value: $value, in: range) {
            HStack {
                Text(label)
                Spacer()
                Text("\(value)")
                    .foregroundStyle(.secondary)
                    .monospacedDigit()
            }
        }
    }
}

private struct LabeledTextField: View {
    let label: String
    @Binding var text: String
    let placeholder: String

    var body: some View {
        HStack {
            Text(label)
                .frame(width: 130, alignment: .leading)
            TextField(placeholder, text: $text)
                .foregroundStyle(.secondary)
        }
    }
}
