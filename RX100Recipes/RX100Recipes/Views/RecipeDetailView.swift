import SwiftUI
import SwiftData
import PhotosUI

struct RecipeDetailView: View {
    let recipe: Recipe

    @Environment(\.modelContext) private var context
    @Query(sort: \RecipeNote.date, order: .reverse) private var allNotes: [RecipeNote]
    @Query private var allFavorites: [RecipeFavorite]
    @Query(sort: \RecipeUserPhoto.date, order: .forward) private var allUserPhotos: [RecipeUserPhoto]

    @State private var showAddNote = false
    @State private var showAdvancedSettings = false
    @State private var selectedPhotoItem: PhotosPickerItem?

    private var recipeNotes: [RecipeNote] {
        allNotes.filter { $0.recipeId == recipe.id }
    }

    private var recipeUserPhotos: [RecipeUserPhoto] {
        allUserPhotos.filter { $0.recipeId == recipe.id }
    }

    private var isFavorite: Bool {
        allFavorites.contains { $0.recipeId == recipe.id }
    }

    var body: some View {
        List {
            // Header Section
            Section {
                headerView
            }
            .listRowBackground(Color.clear)
            .listRowInsets(EdgeInsets(top: 8, leading: 16, bottom: 8, trailing: 16))

            // Sample Photo Section
            if recipe.samplePhotoAssetName != nil || recipe.samplePhotoURL != nil {
                Section {
                    samplePhotoSection
                } header: {
                    Text("Sample Photo")
                        .font(.subheadline.weight(.semibold))
                }
                .listRowBackground(Color.clear)
                .listRowInsets(EdgeInsets(top: 4, leading: 16, bottom: 4, trailing: 16))
            }

            // Settings Section
            Section {
                if let cs = recipe.creativeStyleSettings {
                    creativeStyleView(cs)
                } else if let pp = recipe.pictureProfileSettings {
                    pictureProfileView(pp)
                }
            } header: {
                Text("Camera Settings")
                    .font(.subheadline.weight(.semibold))
            }

            // My Shots Section
            myShotsSection

            // Notes Section
            notesSection
        }
        .listStyle(.insetGrouped)
        .navigationTitle(recipe.name)
        .navigationBarTitleDisplayMode(.inline)
        .toolbar {
            ToolbarItem(placement: .topBarTrailing) {
                Button {
                    toggleFavorite()
                } label: {
                    Image(systemName: isFavorite ? "star.fill" : "star")
                        .foregroundStyle(isFavorite ? .yellow : .secondary)
                        .imageScale(.large)
                }
            }
        }
        .sheet(isPresented: $showAddNote) {
            AddNoteView(recipeId: recipe.id, recipeName: recipe.name)
        }
        .onChange(of: selectedPhotoItem) { _, newItem in
            guard let newItem else { return }
            Task {
                if let data = try? await newItem.loadTransferable(type: Data.self) {
                    let compressed = UIImage(data: data).flatMap {
                        $0.jpegData(compressionQuality: 0.8)
                    } ?? data
                    context.insert(RecipeUserPhoto(recipeId: recipe.id, imageData: compressed))
                }
                selectedPhotoItem = nil
            }
        }
    }

    // MARK: - Header

    private var headerView: some View {
        VStack(alignment: .leading, spacing: 10) {
            HStack(spacing: 6) {
                CategoryBadge(category: recipe.category)
                SettingTypeBadge(type: recipe.settingType)
                Spacer()
            }

            Text(recipe.description)
                .font(.body)
                .foregroundStyle(.primary)

            if let url = recipe.sourceURL {
                Link(destination: url) {
                    HStack(spacing: 4) {
                        Image(systemName: "link.circle")
                            .font(.caption)
                        Text("Source: \(recipe.source)")
                            .font(.caption)
                    }
                    .foregroundStyle(Color.accentColor)
                }
            } else if !recipe.source.isEmpty {
                HStack(spacing: 4) {
                    Image(systemName: "info.circle")
                        .font(.caption)
                        .foregroundStyle(.secondary)
                    Text("Source: \(recipe.source)")
                        .font(.caption)
                        .foregroundStyle(.secondary)
                }
            }

            if !recipe.tags.isEmpty {
                FlowLayout(spacing: 5) {
                    ForEach(recipe.tags, id: \.self) { tag in
                        Text(tag)
                            .font(.caption2)
                            .padding(.horizontal, 8)
                            .padding(.vertical, 3)
                            .background(Color(.tertiarySystemFill))
                            .clipShape(Capsule())
                            .foregroundStyle(.secondary)
                    }
                }
            }
        }
    }

    // MARK: - Sample Photo

    @ViewBuilder
    private var samplePhotoSection: some View {
        if let assetName = recipe.samplePhotoAssetName {
            Image(assetName)
                .resizable()
                .aspectRatio(contentMode: .fill)
                .frame(maxHeight: 240)
                .clipped()
                .clipShape(RoundedRectangle(cornerRadius: 10))
        } else if let url = recipe.samplePhotoURL {
            AsyncImage(url: url) { phase in
                switch phase {
                case .success(let image):
                    image
                        .resizable()
                        .aspectRatio(contentMode: .fill)
                        .frame(maxHeight: 240)
                        .clipped()
                        .clipShape(RoundedRectangle(cornerRadius: 10))
                case .failure:
                    HStack {
                        Image(systemName: "photo")
                            .foregroundStyle(.secondary)
                        Text("Unable to load photo")
                            .font(.caption)
                            .foregroundStyle(.secondary)
                    }
                    .frame(maxWidth: .infinity, minHeight: 60)
                case .empty:
                    RoundedRectangle(cornerRadius: 10)
                        .fill(Color(.secondarySystemFill))
                        .frame(height: 160)
                        .overlay(ProgressView())
                @unknown default:
                    EmptyView()
                }
            }
        }
    }

    // MARK: - Creative Style Settings

    @ViewBuilder
    private func creativeStyleView(_ s: CreativeStyleSettings) -> some View {
        SettingRow(label: "Base Style", value: s.style, isHighlighted: true)
        SettingRow(label: "Contrast", value: s.contrast.signedString)
        SettingRow(label: "Saturation", value: s.saturation.signedString)
        SettingRow(label: "Sharpness", value: s.sharpness.signedString)
        SettingRow(label: "White Balance", value: s.whiteBalance)
        if let shift = s.wbShift { SettingRow(label: "WB Color Filter", value: shift) }
        SettingRow(label: "ISO", value: s.iso)
        SettingRow(label: "Exposure Comp", value: s.exposureComp)

        infoBox(
            icon: "lightbulb.fill",
            text: "Creative Style: MENU → Camera Settings 1 → Creative Style. Adjust Contrast, Saturation, and Sharpness with the dial (−3 to +3). Set White Balance and ISO separately."
        )
    }

    // MARK: - Picture Profile Settings

    @ViewBuilder
    private func pictureProfileView(_ s: PictureProfileSettings) -> some View {
        SettingRow(label: "Profile Slot", value: s.profileSlot, isHighlighted: true)
        SettingRow(label: "Gamma", value: s.gamma)
        SettingRow(label: "Color Mode", value: s.colorMode)
        SettingRow(label: "Black Level", value: s.blackLevel.signedString)
        SettingRow(label: "Saturation", value: s.saturation.signedString)
        SettingRow(label: "Color Phase", value: s.colorPhase.signedString)
        SettingRow(label: "Detail Level", value: s.detailLevel.signedString)
        SettingRow(label: "White Balance", value: s.whiteBalance)
        if let shift = s.wbShift { SettingRow(label: "WB Color Filter", value: shift) }
        SettingRow(label: "ISO", value: s.iso)
        SettingRow(label: "Exposure Comp", value: s.exposureComp)

        if s.hasAdvancedSettings {
            DisclosureGroup(
                isExpanded: $showAdvancedSettings,
                content: {
                    advancedSettingsView(s)
                },
                label: {
                    Label("Advanced Settings", systemImage: "slider.horizontal.3")
                        .font(.subheadline)
                        .foregroundStyle(.secondary)
                }
            )
        }

        infoBox(
            icon: "lightbulb.fill",
            text: "Picture Profile: MENU → Camera Settings 1 → Picture Profile. Select \(s.profileSlot) and adjust each parameter. White Balance is set separately — it cannot be saved per profile."
        )
    }

    @ViewBuilder
    private func advancedSettingsView(_ s: PictureProfileSettings) -> some View {
        // Black Gamma
        if let range = s.blackGammaRange { SettingRow(label: "BG Range", value: range) }
        if let level = s.blackGammaLevel { SettingRow(label: "BG Level", value: level.signedString) }
        // Knee
        if let km = s.kneeMode {
            SettingRow(label: "Knee Mode", value: km)
            if km == "Auto", let sens = s.kneeAutoSensitivity {
                SettingRow(label: "Knee Sensitivity", value: sens)
            } else if km == "Manual" {
                if let pt = s.kneeManualPoint   { SettingRow(label: "Knee Point", value: pt) }
                if let sl = s.kneeManualSlope   { SettingRow(label: "Knee Slope", value: sl.signedString) }
            }
        }
        // Color Depth
        if let r = s.colorDepthR { SettingRow(label: "Color Depth R", value: r.signedString) }
        if let g = s.colorDepthG { SettingRow(label: "Color Depth G", value: g.signedString) }
        if let b = s.colorDepthB { SettingRow(label: "Color Depth B", value: b.signedString) }
        if let c = s.colorDepthC { SettingRow(label: "Color Depth C", value: c.signedString) }
        if let m = s.colorDepthM { SettingRow(label: "Color Depth M", value: m.signedString) }
        if let y = s.colorDepthY { SettingRow(label: "Color Depth Y", value: y.signedString) }
        // Detail sub-settings
        if let dm = s.detailMode         { SettingRow(label: "Detail Mode", value: dm) }
        if let vh = s.detailVHBalance    { SettingRow(label: "Detail V/H Bal", value: vh.signedString) }
        if let bw = s.detailBWBalance    { SettingRow(label: "Detail B/W Bal", value: bw) }
        if let lm = s.detailLimit        { SettingRow(label: "Detail Limit", value: "\(lm)") }
        if let cr = s.detailCrispening   { SettingRow(label: "Crispening", value: "\(cr)") }
        if let hl = s.detailHighLightDetail { SettingRow(label: "H-Light Detail", value: "\(hl)") }
    }

    // MARK: - Info Box

    @ViewBuilder
    private func infoBox(icon: String, text: String) -> some View {
        HStack(alignment: .top, spacing: 8) {
            Image(systemName: icon)
                .foregroundStyle(.orange)
                .font(.footnote)
            Text(text)
                .font(.footnote)
                .foregroundStyle(.secondary)
        }
        .padding(10)
        .background(Color.orange.opacity(0.08))
        .clipShape(RoundedRectangle(cornerRadius: 8))
        .listRowBackground(Color.clear)
        .listRowInsets(EdgeInsets(top: 4, leading: 16, bottom: 4, trailing: 16))
    }

    // MARK: - My Shots Section

    private var myShotsSection: some View {
        Section {
            ForEach(recipeUserPhotos) { photo in
                if let uiImage = UIImage(data: photo.imageData) {
                    Image(uiImage: uiImage)
                        .resizable()
                        .aspectRatio(contentMode: .fill)
                        .frame(height: 200)
                        .clipped()
                        .clipShape(RoundedRectangle(cornerRadius: 8))
                        .listRowInsets(EdgeInsets(top: 4, leading: 16, bottom: 4, trailing: 16))
                }
            }
            .onDelete { indexSet in
                for index in indexSet {
                    context.delete(recipeUserPhotos[index])
                }
            }

            PhotosPicker(selection: $selectedPhotoItem, matching: .images) {
                Label("Add My Shot", systemImage: "camera.fill")
                    .foregroundStyle(Color.accentColor)
            }
        } header: {
            HStack {
                Text("My Shots")
                    .font(.subheadline.weight(.semibold))
                if !recipeUserPhotos.isEmpty {
                    Text("(\(recipeUserPhotos.count))")
                        .font(.subheadline)
                        .foregroundStyle(.secondary)
                }
                Spacer()
            }
        } footer: {
            if recipeUserPhotos.isEmpty {
                Text("Save your own photos shot with this recipe.")
                    .font(.caption)
            }
        }
    }

    // MARK: - Notes Section

    private var notesSection: some View {
        Section {
            ForEach(recipeNotes) { note in
                VStack(alignment: .leading, spacing: 4) {
                    Text(note.text)
                        .font(.subheadline)
                    Text(note.date.formatted(date: .abbreviated, time: .shortened))
                        .font(.caption2)
                        .foregroundStyle(.secondary)
                }
                .padding(.vertical, 2)
            }
            .onDelete { indexSet in
                for index in indexSet {
                    context.delete(recipeNotes[index])
                }
            }

            Button {
                showAddNote = true
            } label: {
                Label("Add Note", systemImage: "plus.circle.fill")
                    .foregroundStyle(Color.accentColor)
            }
        } header: {
            HStack {
                Text("My Notes")
                    .font(.subheadline.weight(.semibold))
                if !recipeNotes.isEmpty {
                    Text("(\(recipeNotes.count))")
                        .font(.subheadline)
                        .foregroundStyle(.secondary)
                }
                Spacer()
            }
        } footer: {
            if recipeNotes.isEmpty {
                Text("Capture your observations after shooting with this recipe.")
                    .font(.caption)
            }
        }
    }

    // MARK: - Actions

    private func toggleFavorite() {
        if let existing = allFavorites.first(where: { $0.recipeId == recipe.id }) {
            context.delete(existing)
        } else {
            context.insert(RecipeFavorite(recipeId: recipe.id))
        }
    }
}

// MARK: - Flow Layout (for tags)

struct FlowLayout: Layout {
    var spacing: CGFloat = 8

    func sizeThatFits(proposal: ProposedViewSize, subviews: Subviews, cache: inout ()) -> CGSize {
        let width = proposal.width ?? 0
        var height: CGFloat = 0
        var x: CGFloat = 0
        var rowHeight: CGFloat = 0

        for subview in subviews {
            let size = subview.sizeThatFits(.unspecified)
            if x + size.width > width && x > 0 {
                height += rowHeight + spacing
                x = 0
                rowHeight = 0
            }
            x += size.width + spacing
            rowHeight = max(rowHeight, size.height)
        }
        height += rowHeight
        return CGSize(width: width, height: height)
    }

    func placeSubviews(in bounds: CGRect, proposal: ProposedViewSize, subviews: Subviews, cache: inout ()) {
        var x = bounds.minX
        var y = bounds.minY
        var rowHeight: CGFloat = 0

        for subview in subviews {
            let size = subview.sizeThatFits(.unspecified)
            if x + size.width > bounds.maxX && x > bounds.minX {
                y += rowHeight + spacing
                x = bounds.minX
                rowHeight = 0
            }
            subview.place(at: CGPoint(x: x, y: y), proposal: ProposedViewSize(size))
            x += size.width + spacing
            rowHeight = max(rowHeight, size.height)
        }
    }
}
