import SwiftUI
import SwiftData

struct PPSlotsView: View {
    @Environment(RecipeStore.self) private var store
    @Environment(\.modelContext) private var context
    @Query(sort: \PPSlotAssignment.slot) private var assignments: [PPSlotAssignment]
    @State private var editingSlot: Int?

    var body: some View {
        NavigationStack {
            List {
                Section {
                    ForEach(1...10, id: \.self) { slot in
                        SlotRow(
                            slot: slot,
                            assignment: assignments.first { $0.slot == slot }
                        ) {
                            editingSlot = slot
                        } onClear: {
                            if let a = assignments.first(where: { $0.slot == slot }) {
                                context.delete(a)
                            }
                        }
                    }
                } header: {
                    Text("Tap a slot to assign or change a recipe")
                        .textCase(nil)
                } footer: {
                    Text("Only Picture Profile recipes can be assigned to PP slots. Each recipe can occupy only one slot.")
                }
            }
            .listStyle(.insetGrouped)
            .navigationTitle("PP Slots")
            .sheet(item: Binding(
                get: { editingSlot.map { SlotID(id: $0) } },
                set: { editingSlot = $0?.id }
            )) { slotID in
                SlotPickerView(
                    slot: slotID.id,
                    currentRecipeId: assignments.first(where: { $0.slot == slotID.id })?.recipeId,
                    ppRecipes: store.allRecipes.filter { $0.settingType == .pictureProfile }
                ) { selected in
                    // Remove existing assignment for this slot
                    if let existing = assignments.first(where: { $0.slot == slotID.id }) {
                        context.delete(existing)
                    }
                    if let recipe = selected {
                        // Also remove recipe from any other slot it currently occupies
                        if let otherSlot = assignments.first(where: {
                            $0.recipeId == recipe.id && $0.slot != slotID.id
                        }) {
                            context.delete(otherSlot)
                        }
                        context.insert(PPSlotAssignment(
                            slot: slotID.id,
                            recipeId: recipe.id,
                            recipeName: recipe.name
                        ))
                    }
                    editingSlot = nil
                }
            }
        }
    }
}

// MARK: - Slot Row

private struct SlotRow: View {
    let slot: Int
    let assignment: PPSlotAssignment?
    let onTap: () -> Void
    let onClear: () -> Void

    var body: some View {
        HStack {
            Text("PP\(slot)")
                .font(.headline.monospacedDigit())
                .foregroundStyle(.secondary)
                .frame(width: 44, alignment: .leading)

            if let assignment {
                VStack(alignment: .leading, spacing: 2) {
                    Text(assignment.recipeName)
                        .font(.subheadline)
                }
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
        .onTapGesture { onTap() }
        .swipeActions(edge: .trailing) {
            if assignment != nil {
                Button(role: .destructive, action: onClear) {
                    Label("Clear", systemImage: "xmark")
                }
            }
        }
    }
}

// MARK: - Slot Picker Sheet

private struct SlotID: Identifiable { let id: Int }

private struct SlotPickerRow: View {
    let recipe: Recipe
    let isSelected: Bool
    let onSelect: (Recipe?) -> Void

    var body: some View {
        HStack {
            VStack(alignment: .leading, spacing: 3) {
                Text(recipe.name)
                    .font(.subheadline.weight(.medium))
                if let pp = recipe.pictureProfileSettings {
                    Text("\(pp.gamma) · \(pp.colorMode)")
                        .font(.caption)
                        .foregroundStyle(.secondary)
                }
            }
            Spacer()
            if isSelected {
                Image(systemName: "checkmark")
                    .foregroundStyle(.accentColor)
                    .font(.footnote.weight(.semibold))
            }
        }
        .contentShape(Rectangle())
        .onTapGesture { onSelect(recipe) }
    }
}

private struct SlotPickerView: View {
    let slot: Int
    let currentRecipeId: UUID?
    let ppRecipes: [Recipe]
    let onSelect: (Recipe?) -> Void
    @Environment(\.dismiss) private var dismiss

    var body: some View {
        NavigationStack {
            List {
                Section {
                    Button(role: .destructive) {
                        onSelect(nil)
                    } label: {
                        Label("Clear Slot PP\(slot)", systemImage: "xmark.circle")
                    }
                }

                Section("Picture Profile Recipes") {
                    ForEach(ppRecipes.sorted { $0.name < $1.name }) { recipe in
                        SlotPickerRow(
                            recipe: recipe,
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
