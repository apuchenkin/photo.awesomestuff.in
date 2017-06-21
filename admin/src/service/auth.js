const AUTH = 'auth';

const getToken = () => localStorage.getItem(AUTH) || '';

const login = (user, pass) => {
  const auth = window.btoa([user, pass].join(':'));
  localStorage.setItem(AUTH, `Basic ${auth}`);
  return Promise.resolve();
};

const logout = () => {
  localStorage.removeItem(AUTH);

  return Promise.resolve();
};

export default {
  getToken,
  logout,
  login,
};
