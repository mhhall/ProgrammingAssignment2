import SwiftUI
import SwiftData

struct NotesListView: View {
    @Environment(RecipeStore.self) private var store
    @Environment(\.modelContext) private var context
    @Query(sort: \RecipeNote.date, order: .reverse) private var allNotes: [RecipeNote]

    @State private var noteToDelete: RecipeNote? = nil

    private func recipeName(for note: RecipeNote) -> String {
        store.allRecipes.first { $0.id == note.recipeId }?.name ?? "Unknown Recipe"
    }

    private func recipe(for note: RecipeNote) -> Recipe? {
        store.allRecipes.first { $0.id == note.recipeId }
    }

    var body: some View {
        NavigationStack {
            Group {
                if allNotes.isEmpty {
                    ContentUnavailableView(
                        "No Notes Yet",
                        systemImage: "note.text",
                        description: Text("Open any recipe and tap \"Add Note\" to capture your shooting observations.")
                    )
                } else {
                    List {
                        ForEach(allNotes) { note in
                            noteRow(note)
                        }
                        .onDelete { indexSet in
                            for index in indexSet {
                                context.delete(allNotes[index])
                            }
                        }
                    }
                    .listStyle(.insetGrouped)
                }
            }
            .navigationTitle("My Notes")
        }
    }

    @ViewBuilder
    private func noteRow(_ note: RecipeNote) -> some View {
        if let recipe = recipe(for: note) {
            NavigationLink {
                RecipeDetailView(recipe: recipe)
            } label: {
                VStack(alignment: .leading, spacing: 5) {
                    HStack {
                        CategoryBadge(category: recipe.category)
                        Text(recipe.name)
                            .font(.caption.weight(.semibold))
                            .foregroundStyle(.secondary)
                    }
                    Text(note.text)
                        .font(.subheadline)
                        .lineLimit(3)
                    Text(note.date.formatted(date: .abbreviated, time: .shortened))
                        .font(.caption2)
                        .foregroundStyle(.tertiary)
                }
                .padding(.vertical, 2)
            }
        } else {
            VStack(alignment: .leading, spacing: 4) {
                Text(note.text)
                    .font(.subheadline)
                    .lineLimit(3)
                Text(note.date.formatted(date: .abbreviated, time: .shortened))
                    .font(.caption2)
                    .foregroundStyle(.tertiary)
            }
            .padding(.vertical, 2)
        }
    }
}
