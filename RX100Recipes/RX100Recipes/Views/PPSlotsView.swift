import SwiftUI
import SwiftData

// MARK: - Camera Memory View

private enum SlotMode { case pp, mr }

struct PPSlotsView: View {
    @Environment(RecipeStore.self) private var store
    @Environment(\.modelContext) private var context
    @Query(sort: \PPSlotAssignment.slot) private var ppAssignments: [PPSlotAssignment]
    @Query private var mrAssignments: [MRSlotAssignment]

    @State private var mode: SlotMode = .pp
    @State private var editingPPSlot: Int?
    @State private var editingMRSlot: String?
    @State private var infoRecipe: Recipe?

    private let mrSlots = ["1", "2", "3", "M1", "M2", "M3", "M4"]

    var body: some View {
        NavigationStack {
            Group {
                if mode == .pp { ppList } else { mrList }
            }
            .navigationTitle("Camera Memory")
            .navigationBarTitleDisplayMode(.inline)
            .toolbar {
                ToolbarItem(placement: .principal) {
                    Picker("", selection: $mode) {
                        Text("PP Slots").tag(SlotMode.pp)
                        Text("MR Slots").tag(SlotMode.mr)
                    }
                    .pickerStyle(.segmented)
                    .frame(width: 200)
                }
            }
            .navigationDestination(for: Recipe.self) { recipe in
                RecipeDetailView(recipe: recipe)
            }
            // PP slot assignment picker
            .sheet(item: Binding(
                get: { editingPPSlot.map { PPSlotID(id: $0) } },
                set: { editingPPSlot = $0?.id }
            )) { slotID in
                PPTabSlotPickerView(
                    slot: slotID.id,
                    currentRecipeId: ppAssignments.first { $0.slot == slotID.id }?.recipeId,
                    recipes: store.allRecipes.filter { $0.settingType == .pictureProfile }
                ) { selected in
                    if let existing = ppAssignments.first(where: { $0.slot == slotID.id }) {
                        context.delete(existing)
                    }
                    if let recipe = selected {
                        if let other = ppAssignments.first(where: {
                            $0.recipeId == recipe.id && $0.slot != slotID.id
                        }) { context.delete(other) }
                        context.insert(PPSlotAssignment(
                            slot: slotID.id, recipeId: recipe.id, recipeName: recipe.name
                        ))
                    }
                    editingPPSlot = nil
                }
            }
            // MR slot assignment picker
            .sheet(item: Binding(
                get: { editingMRSlot.map { MRSlotID(id: $0) } },
                set: { editingMRSlot = $0?.id }
            )) { slotID in
                MRTabSlotPickerView(
                    slot: slotID.id,
                    currentRecipeId: mrAssignments.first { $0.slot == slotID.id }?.recipeId,
                    recipes: store.allRecipes.filter { $0.settingType == .creativeStyle }
                ) { selected in
                    if let existing = mrAssignments.first(where: { $0.slot == slotID.id }) {
                        context.delete(existing)
                    }
                    if let recipe = selected {
                        if let other = mrAssignments.first(where: {
                            $0.recipeId == recipe.id && $0.slot != slotID.id
                        }) { context.delete(other) }
                        context.insert(MRSlotAssignment(
                            slot: slotID.id, recipeId: recipe.id, recipeName: recipe.name
                        ))
                    }
                    editingMRSlot = nil
                }
            }
            // Info sheet → recipe detail
            .sheet(item: $infoRecipe) { recipe in
                NavigationStack { RecipeDetailView(recipe: recipe) }
            }
        }
    }

    // MARK: - PP List

    private var ppList: some View {
        List {
            Section {
                ForEach(1...10, id: \.self) { slot in
                    let assignment = ppAssignments.first { $0.slot == slot }
                    let recipe = assignment.flatMap { a in store.allRecipes.first { $0.id == a.recipeId } }
                    CameraSlotRow(slotLabel: "PP\(slot)", recipeName: assignment?.recipeName) {
                        editingPPSlot = slot
                    } onClear: {
                        if let a = assignment { context.delete(a) }
                    } onInfo: {
                        infoRecipe = recipe
                    }
                }
            } header: {
                Text("Tap a slot to assign or change a recipe").textCase(nil)
            } footer: {
                Text("Only Picture Profile recipes can be assigned to PP slots. Each recipe can occupy only one slot.")
            }
        }
        .listStyle(.insetGrouped)
    }

    // MARK: - MR List

    private var mrList: some View {
        List {
            Section {
                ForEach(mrSlots, id: \.self) { slot in
                    let assignment = mrAssignments.first { $0.slot == slot }
                    let recipe = assignment.flatMap { a in store.allRecipes.first { $0.id == a.recipeId } }
                    CameraSlotRow(slotLabel: "MR\(slot)", recipeName: assignment?.recipeName) {
                        editingMRSlot = slot
                    } onClear: {
                        if let a = assignment { context.delete(a) }
                    } onInfo: {
                        infoRecipe = recipe
                    }
                }
            } header: {
                Text("Tap a slot to assign or change a recipe").textCase(nil)
            } footer: {
                Text("Only Creative Style recipes can be assigned to MR slots. Each recipe can occupy only one slot.")
            }
        }
        .listStyle(.insetGrouped)
    }
}

// MARK: - Shared Slot Row

private struct CameraSlotRow: View {
    let slotLabel: String
    let recipeName: String?
    let onTap: () -> Void
    let onClear: () -> Void
    let onInfo: () -> Void

    var body: some View {
        HStack(spacing: 0) {
            Button(action: onTap) {
                HStack {
                    Text(slotLabel)
                        .font(.headline.monospacedDigit())
                        .foregroundStyle(.secondary)
                        .frame(width: 52, alignment: .leading)

                    if let recipeName {
                        Text(recipeName)
                            .font(.subheadline)
                            .foregroundStyle(.primary)
                    } else {
                        Text("Empty")
                            .font(.subheadline)
                            .foregroundStyle(.tertiary)
                            .italic()
                    }

                    Spacer()

                    Image(systemName: "chevron.right")
                        .font(.caption2.weight(.semibold))
                        .foregroundStyle(.tertiary)
                }
                .contentShape(Rectangle())
            }
            .buttonStyle(.plain)

            if recipeName != nil {
                Button(action: onInfo) {
                    Image(systemName: "info.circle")
                        .foregroundStyle(.secondary)
                        .font(.body)
                        .padding(.leading, 12)
                        .contentShape(Rectangle())
                }
                .buttonStyle(.plain)
            }
        }
        .swipeActions(edge: .trailing) {
            if recipeName != nil {
                Button(role: .destructive, action: onClear) {
                    Label("Clear", systemImage: "xmark")
                }
            }
        }
    }
}

// MARK: - Identifiable slot wrappers

private struct PPSlotID: Identifiable { let id: Int }
private struct MRSlotID: Identifiable { let id: String }

// MARK: - Shared Recipe Picker Row

private struct RecipePickerRow: View {
    let recipe: Recipe
    let subtitle: String?
    let isSelected: Bool
    let onSelect: (Recipe?) -> Void

    var body: some View {
        HStack {
            VStack(alignment: .leading, spacing: 2) {
                Text(recipe.name)
                    .font(.subheadline.weight(.medium))
                if let subtitle {
                    Text(subtitle)
                        .font(.caption)
                        .foregroundStyle(.secondary)
                }
            }
            Spacer()
            if isSelected {
                Image(systemName: "checkmark")
                    .foregroundStyle(Color.accentColor)
                    .font(.footnote.weight(.semibold))
            }
        }
        .contentShape(Rectangle())
        .onTapGesture { onSelect(recipe) }
    }
}

// MARK: - PP Tab Slot Picker

private struct PPTabSlotPickerView: View {
    let slot: Int
    let currentRecipeId: UUID?
    let recipes: [Recipe]
    let onSelect: (Recipe?) -> Void
    @Environment(\.dismiss) private var dismiss

    var body: some View {
        NavigationStack {
            List {
                Section {
                    Button(role: .destructive) { onSelect(nil) } label: {
                        Label("Clear Slot PP\(slot)", systemImage: "xmark.circle")
                    }
                }
                Section("Picture Profile Recipes") {
                    ForEach(recipes.sorted { $0.name < $1.name }) { recipe in
                        RecipePickerRow(
                            recipe: recipe,
                            subtitle: recipe.pictureProfileSettings.map { "\($0.gamma) · \($0.colorMode)" },
                            isSelected: recipe.id == currentRecipeId,
                            onSelect: onSelect
                        )
                    }
                }
            }
            .listStyle(.insetGrouped)
            .navigationTitle("Assign PP\(slot)")
            .navigationBarTitleDisplayMode(.inline)
            .toolbar {
                ToolbarItem(placement: .topBarTrailing) {
                    Button("Cancel") { dismiss() }
                }
            }
        }
    }
}

// MARK: - MR Tab Slot Picker

private struct MRTabSlotPickerView: View {
    let slot: String
    let currentRecipeId: UUID?
    let recipes: [Recipe]
    let onSelect: (Recipe?) -> Void
    @Environment(\.dismiss) private var dismiss

    var body: some View {
        NavigationStack {
            List {
                Section {
                    Button(role: .destructive) { onSelect(nil) } label: {
                        Label("Clear Slot MR\(slot)", systemImage: "xmark.circle")
                    }
                }
                Section("Creative Style Recipes") {
                    ForEach(recipes.sorted { $0.name < $1.name }) { recipe in
                        RecipePickerRow(
                            recipe: recipe,
                            subtitle: recipe.creativeStyleSettings.map { $0.style },
                            isSelected: recipe.id == currentRecipeId,
                            onSelect: onSelect
                        )
                    }
                }
            }
            .listStyle(.insetGrouped)
            .navigationTitle("Assign MR\(slot)")
            .navigationBarTitleDisplayMode(.inline)
            .toolbar {
                ToolbarItem(placement: .topBarTrailing) {
                    Button("Cancel") { dismiss() }
                }
            }
        }
    }
}
