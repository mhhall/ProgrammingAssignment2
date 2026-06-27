import SwiftUI
import SwiftData

struct RecipeListView: View {
    @Environment(RecipeStore.self) private var store
    @Query private var favorites: [RecipeFavorite]

    var body: some View {
        @Bindable var store = store
        NavigationStack {
            VStack(spacing: 0) {
                CategoryFilterBar()
                SettingTypeFilterBar()
                recipeList
            }
            .background(Color(.systemGroupedBackground))
            .navigationTitle("RX100 Recipes")
            .searchable(text: $store.searchText, placement: .navigationBarDrawer(displayMode: .always), prompt: "Search recipes, tags, styles…")
            .toolbar {
                ToolbarItem(placement: .topBarTrailing) {
                    sortMenu(store: store)
                }
            }
        }
    }

    private var recipeList: some View {
        List {
            let recipes = store.filteredRecipes
            if recipes.isEmpty {
                ContentUnavailableView(
                    "No Recipes Found",
                    systemImage: "camera.filters",
                    description: Text("Try adjusting your search or filters.")
                )
                .listRowBackground(Color.clear)
            } else {
                ForEach(recipes) { recipe in
                    NavigationLink {
                        RecipeDetailView(recipe: recipe)
                    } label: {
                        RecipeRowView(
                            recipe: recipe,
                            isFavorite: favorites.contains { $0.recipeId == recipe.id }
                        )
                    }
                }
            }
        }
        .listStyle(.insetGrouped)
        .animation(.default, value: store.filteredRecipes.map { $0.id })
    }

    @ViewBuilder
    private func sortMenu(store: RecipeStore) -> some View {
        @Bindable var store = store
        Menu {
            Picker("Sort", selection: $store.sortOption) {
                ForEach(RecipeStore.SortOption.allCases) { option in
                    Text(option.rawValue).tag(option)
                }
            }
        } label: {
            Label("Sort", systemImage: "arrow.up.arrow.down")
        }
    }
}

// MARK: - Category Filter Bar

struct CategoryFilterBar: View {
    @Environment(RecipeStore.self) private var store

    var body: some View {
        ScrollView(.horizontal, showsIndicators: false) {
            HStack(spacing: 8) {
                FilterChip(title: "All", isSelected: store.selectedCategory == nil) {
                    store.selectedCategory = nil
                }

                ForEach(RecipeCategory.allCases) { category in
                    FilterChip(
                        title: category.rawValue,
                        systemImage: category.sfSymbol,
                        isSelected: store.selectedCategory == category
                    ) {
                        store.selectedCategory = store.selectedCategory == category ? nil : category
                    }
                }
            }
            .padding(.horizontal, 16)
            .padding(.vertical, 10)
        }
        .background(Color(.systemGroupedBackground))
    }
}

// MARK: - Setting Type Filter Bar

struct SettingTypeFilterBar: View {
    @Environment(RecipeStore.self) private var store

    var body: some View {
        HStack(spacing: 0) {
            typeButton(title: "All", type: nil)
            Divider().frame(height: 28)
            typeButton(title: SettingType.creativeStyle.rawValue, type: .creativeStyle)
            Divider().frame(height: 28)
            typeButton(title: SettingType.pictureProfile.rawValue, type: .pictureProfile)
        }
        .background(Color(.secondarySystemBackground))
        .clipShape(RoundedRectangle(cornerRadius: 10))
        .padding(.horizontal, 16)
        .padding(.bottom, 10)
        .background(Color(.systemGroupedBackground))
    }

    @ViewBuilder
    private func typeButton(title: String, type: SettingType?) -> some View {
        let isSelected = store.selectedSettingType == type
        Button {
            withAnimation(.spring(duration: 0.2)) {
                store.selectedSettingType = type
            }
        } label: {
            Text(title)
                .font(.caption.weight(.semibold))
                .lineLimit(1)
                .minimumScaleFactor(0.8)
                .padding(.vertical, 8)
                .frame(maxWidth: .infinity)
                .background(isSelected ? Color.accentColor : Color.clear)
                .foregroundStyle(isSelected ? Color.white : Color.secondary)
        }
        .buttonStyle(.plain)
    }
}
