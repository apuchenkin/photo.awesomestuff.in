import * as React from 'react';

type SetPage = (page: Page) => void;
type GetPages = () => Page[];
type GetPage = (alias: string) => Page;

interface PageProvider {
  setPage: SetPage;
  getPages: GetPages;
  getPage: GetPage;
}

interface State {
  pages: Record<string, Page>
}

const SET_PAGE = 'SET_PAGE';

interface SetPageAction {
  type: 'SET_PAGE';
  page: Page;
}

interface Props {
  pages: Page[];
}

// @ts-ignore
export const Context = React.createContext<PageProvider>();

const reducer: React.Reducer<State, SetPageAction> = (state, action) => {
  switch (action.type) {
    case SET_PAGE:
      return {
        ...state,
        pages: {
          ...state.pages,
          [action.page.alias]: action.page,
        }
      };
    default:
      throw new Error();
  }
}

const PageProvider: React.FunctionComponent<Props> = ({ pages, children }) => {
  const initialState = {
    pages: pages.reduce((acc, page) => ({
      ...acc,
      [page.alias]: page,
    }), {})
  };

  const [state, dispatch] = React.useReducer(reducer, initialState);

  const getPages = React.useCallback(
    () => Object.values(state.pages),
    [state],
  );

  const getPage = React.useCallback(
    (alias: string) => state.pages[alias],
    [state],
  );

  const setPage = (page: Page) => dispatch({ type: SET_PAGE, page });

  return (
    <Context.Provider
      value={{
        setPage,
        getPages,
        getPage,
      }}
    >
      {children}
    </Context.Provider>
  );
}

export default PageProvider;
